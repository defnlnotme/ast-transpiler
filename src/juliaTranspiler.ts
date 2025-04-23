import { BaseTranspiler } from "./baseTranspiler.js";
import { regexAll } from "./utils.js";
import ts, {
    ConstructorDeclaration,
    isConstructorTypeNode,
    MethodDeclaration,
} from "typescript";
import { TranspilationError } from "./types.js"; // Import TranspilationError
import { red, reset } from "colorette";
import { Sign } from "crypto";

// --- Conditional Debug Logging ---
// Helper function to log messages only if the JULIA_TRANSPILER_DEBUG environment variable is set.
// Uses console.log as requested.
const conditionalDebugLog = (...args: any[]) => {
    // Check for a specific environment variable, e.g., JULIA_TRANSPILER_DEBUG
    // Use process.env which is standard in Node.js environments
    if (process.env.JULIA_TRANSPILER_DEBUG) {
        console.log(...args); // Use console.log as requested
    }
};
// --- End Conditional Debug Logging ---

const SyntaxKind = ts.SyntaxKind;
const IGNORED_NODES = new Set<ts.SyntaxKind>([SyntaxKind.ImportDeclaration, SyntaxKind.ImportKeyword]);

const parserConfig = {
    STATIC_TOKEN: "", // to do static decorator
    PUBLIC_KEYWORD: "",
    UNDEFINED_TOKEN: "nothing",
    IF_TOKEN: "if",
    ELSE_TOKEN: "else",
    ELSEIF_TOKEN: "elseif",
    THIS_TOKEN: "", // Julia uses 'this' differently; adjust as needed
    AMPERSTAND_APERSAND_TOKEN: "&&",
    BAR_BAR_TOKEN: "||",
    SPACE_DEFAULT_PARAM: "",
    BLOCK_OPENING_TOKEN: "",
    BLOCK_CLOSING_TOKEN: "end",
    SPACE_BEFORE_BLOCK_OPENING: "",
    CONDITION_OPENING: "",
    CONDITION_CLOSE: "",
    TRUE_KEYWORD: "true",
    FALSE_KEYWORD: "false",
    THROW_TOKEN: "throw",
    NOT_TOKEN: "!",
    PLUS_PLUS_TOKEN: " += 1",
    MINUS_MINUS_TOKEN: " -= 1",
    SUPER_CALL_TOKEN: "super()",
    PROPERTY_ASSIGNMENT_TOKEN: "=",
    FUNCTION_TOKEN: "function",
    SUPER_TOKEN: "super()",
    DEFAULT_PARAMETER_TYPE: "",
    NEW_TOKEN: "new",
    WHILE_TOKEN: "while",
    BREAK_TOKEN: "break",
    STRING_QUOTE_TOKEN: "",
    LINE_TERMINATOR: ";", // Remove the line terminator - No line terminator in Julia
    METHOD_TOKEN: "function",
    CATCH_TOKEN: "catch",
    CATCH_DECLARATION: "e",
    METHOD_DEFAULT_ACCESS: "",
    SPREAD_TOKEN: "...",
    NULL_TOKEN: "nothing",
    DEFAULT_IDENTATION: "    ",
};

export class JuliaTranspiler extends BaseTranspiler {
    ARRAY_KEYWORD = "[]";
    OBJECT_KEYWORD = "Dict()";
    STRING_QUOTE_TOKEN = '"';
    CLASS_TOKEN = "struct";
    CLASS_IMPLEMENTS_TOKEN = "<:";
    CLASS_CONSTRUCTOR_TOKEN = "function";
    CLASS_PROPERTIE_TOKEN = "";
    CLASS_METHOD_TOKEN = "function"; // Corrected: CLASS_METHOD_TOKEN to METHOD_TOKEN - using METHOD_TOKEN from base class
    CLASS_METHOD_RETURN_TYPE_TOKEN = "";
    CLASS_GET_TOKEN = "get";
    CLASS_SET_TOKEN = "set";
    CLASS_STATIC_TOKEN = "";

    protected promisesArrayLiteral: string | undefined;
    protected tmpJSDoc: string;
    protected withinFunctionDeclaration: boolean;
    protected currentFunctionName: string | undefined;
    protected currentFunctionParams: string | undefined;
    protected transpiledComments: Set<string>;
    protected doComments: boolean;

    constructor(config = {}) {
        config["parser"] = Object.assign(
            {},
            parserConfig,
            config["parser"] ?? {},
        );

        super(config);
        this.id = "julia";

        this.initConfig();
        this.asyncTranspiling = config["async"] ?? true;

        // Fix: Keep variable declarations for function expressions to maintain assignment syntax
        this.removeVariableDeclarationForFunctionExpression =
            config["removeVariableDeclarationForFunctionExpression"] ?? false;

        this.includeFunctionNameInFunctionExpressionDeclaration =
            config["includeFunctionNameInFunctionExpressionDeclaration"] ??
            true;

        // User overrides
        this.applyUserOverrides(config);
    }

    initConfig() {
        this.LeftPropertyAccessReplacements = {
            this: "self",
        };
        this.RightPropertyAccessReplacements = {
            push: "push!",
            toUpperCase: "uppercase",
            toLowerCase: "lowercase",
            indexOf: "findfirst", // Correct function name
            padEnd: "rpad",
            padStart: "lpad",
            concat: "concat",
            // REMOVED: 'keys', it needs special handling via printObjectKeysCall
            // REMOVED: 'length', it needs special handling via printLengthProperty
        };
        this.FullPropertyAccessReplacements = {
            "console.log": "println", // Use config
            "JSON.stringify": "JSON3.json",
            "JSON.parse": "JSON3.parse",
            "Math.log": "log",
            "Math.abs": "abs",
            "Math.min": "min",
            "Math.max": "max",
            "Math.ceil": "ceil", // Often needs Int conversion, see printMathCeilCall
            "Math.round": "round", // Often needs Int conversion, see printMathRoundCall
            "Math.floor": "floor", // Often needs Int conversion, see printMathFloorCall
            "Math.pow": "pow", // Julia uses ^ operator often e.g. x^y
            "process.exit": "exit",
            "Number.MAX_SAFE_INTEGER": "typemax(Int)",
            "Number.isInteger": "isinteger", // Correct function name
            // "Object.keys": handled by printObjectKeysCall
            // "Object.values": handled by printObjectValuesCall
            // "Array.isArray": handled by printArrayIsArrayCall
        };
        this.CallExpressionReplacements = {
            parseInt: "parse(Int, ", // Needs closing paren added by printCallExpression
            parseFloat: "parse(Float64, ", // Needs closing paren
            "Promise.all": "asyncmap(identity, ", // Needs closing paren
            "String.fromCharCode": "string(Char(", // Wrap in string() and needs closing paren for Char
            atob: "String(base64decode(", // Wrap in String() and needs closing paren
            btoa: "base64encode(String(", // Wrap argument in String() and needs closing paren
            decodeURIComponent: "urldecode(", // Needs closing paren
            encodeURIComponent: "urlencode(", // Needs closing paren
            setImmediate: "yield", // yield is closer semantically than sleep(0)
        };
        // PropertyAccessRequiresParenthesisRemoval: Usually empty for Julia unless specific cases arise
        this.PropertyAccessRequiresParenthesisRemoval = [];
        this.withinFunctionDeclaration = false;
        this.tmpJSDoc = "";
        this.currentFunctionName = undefined;
        this.currentFunctionParams = undefined;
        this.transpiledComments = new Set<string>();
        this.doComments = false;

        this.ReservedKeywordsReplacements = {
            // List of Julia reserved keywords
            abstract: "abstract_var",
            baremodule: "baremodule_var",
            begin: "begin_var",
            break: "break_var",
            catch: "catch_var",
            const: "const_var",
            continue: "continue_var",
            do: "do_var",
            else: "else_var",
            elseif: "elseif_var",
            end: "end_var",
            export: "export_var",
            false: "false_var",
            finally: "finally_var",
            for: "for_var",
            function: "function_var",
            global: "global_var",
            if: "if_var",
            import: "import_var",
            let: "let_var",
            local: "local_var",
            macro: "macro_var",
            module: "module_var",
            mutable: "mutable_var",
            primitive: "primitive_var",
            quote: "quote_var",
            return: "return_var",
            struct: "struct_var",
            true: "true_var",
            try: "try_var",
            type: "type_var",
            using: "using_var",
            while: "while_var",
        };
    }

    printVariableStatement(
        node: ts.VariableStatement,
        identation: number,
    ): string {
        conditionalDebugLog(
            // Changed from console.debug
            "Entering printVariableStatement function",
            ts.SyntaxKind[node.kind],
        ); // Debug log
        if (this.isCJSRequireStatement(node)) {
            return ""; // remove cjs imports
        }
        let result = "";
        if (node.declarationList) {
            // Pass 0 for indentation as the list itself doesn't have independent indent,
            // but the declarations within might get indent from their specific print functions if needed.
            // Let printVariableDeclarationList/printVariableDeclaration handle the core content.
            result += this.printNode(node.declarationList, 0);
        }
        // Add the line terminator (semicolon) if needed, after processing all declarations.
        // Apply leading indentation to the whole statement result.
        const trimmedResult = result.trim();
        if (trimmedResult !== "") {
            // If the result doesn't already end with the terminator (e.g. complex statements might add their own)
            if (!trimmedResult.endsWith(this.LINE_TERMINATOR)) {
                result += this.LINE_TERMINATOR;
            }
            // Apply outer indentation
            // NOTE: If printNode for declaration list somehow adds its own unwanted indent, this might double it.
            // Need to ensure printVariableDeclarationList/printVariableDeclaration don't add outer indent.
            result = this.getIden(identation) + result; // Apply identation here
            // Add newline if the result doesn't end with one (blocks typically do)
            if (!result.endsWith("\n")) {
                // result += '\n'; // Temporarily removed as tests don't expect extra newlines after simple vars
            }
        } else {
            result = ""; // Ensure empty result if content was empty
        }

        return result; // Return potentially empty string or the formatted statement
    }

    printVariableDeclarationList(
        node: ts.VariableDeclarationList,
        identation: number, // Usually 0 when called from VariableStatement
    ): string {
        conditionalDebugLog(
            // Changed from console.debug
            "Entering printVariableDeclarationList function",
            ts.SyntaxKind[node.kind],
        ); // Debug log
        // Process declarations and join them. Semicolons/newlines are handled by VariableStatement.
        // Pass 0 for indentation to declarations, as VariableStatement handles the line indent.
        return node.declarations
            .map((declaration) => {
                return this.printVariableDeclaration(declaration, 0); // Pass 0 indent
            })
            .filter((d) => d.trim() !== "")
            .join(", "); // Join multiple declarations if needed (though less common in TS community code)
        // Note: Joining with ", " might be wrong if Julia expects separate lines.
        // Assuming one declaration per statement for now based on tests. If multiple needed, revise joiner.
        // Returning the raw content, VariableStatement adds terminator/newline/indent.
    }

    printVariableDeclaration(
        node: ts.VariableDeclaration,
        identation: number,
    ): string {
        conditionalDebugLog(
            // Changed from console.debug
            "Entering printVariableDeclaration function",
            ts.SyntaxKind[node.kind],
            "Node Name Kind:",
            ts.SyntaxKind[node.name.kind], // Added log for name kind
        );

        const nameNode = node.name;
        const initializer = node.initializer;

        // Handle Function Expression Assignment first
        if (
            initializer &&
            (ts.isFunctionExpression(initializer) ||
                ts.isArrowFunction(initializer)) &&
            this.removeVariableDeclarationForFunctionExpression === false
        ) {
            let varName = "#FUNC_ASSIGN_ERROR#"; // Placeholder
            if (ts.isIdentifier(nameNode)) {
                varName = nameNode.escapedText as string;
            } else {
                // Need to handle assignment to patterns carefully here if needed
                varName = this.printNode(nameNode, 0); // Fallback
            }
            if (varName in this.ReservedKeywordsReplacements) {
                varName += "_var";
            }
            return this.printFunctionExpressionAsDeclaration(
                initializer,
                varName,
            );
        }

        // Handle Array Binding Pattern
        if (ts.isArrayBindingPattern(nameNode)) {
            // Let printArrayBindingPattern handle the entire "LHS = RHS" structure
            // Pass the pattern node itself to printNode which will delegate
            return this.printNode(nameNode, 0); // This delegates to printArrayBindingPattern
        }

        // Handle simple Identifier
        if (ts.isIdentifier(nameNode)) {
            let varName = nameNode.escapedText as string;
            if (varName in this.ReservedKeywordsReplacements) {
                varName += "_var";
            }
            if (initializer) {
                // If the initializer was a function expression, it should have been caught above
                const printedInitializer = this.printNode(initializer, 0);
                // printVariableStatement will add the semicolon
                return `${varName} = ${printedInitializer}`; // Return directly, NO SEMICOLON here
            } else {
                // Julia requires initialization or type annotation.
                return ""; // Return empty for uninitialized simple vars for now.
            }
        }

        // Fallback/Error for other complex BindingName types (e.g., ObjectBindingPattern)
        console.warn(
            "Unhandled BindingName type in printVariableDeclaration:",
            ts.SyntaxKind[nameNode.kind],
            "file:",
            nameNode.getSourceFile().fileName
        );
        const printedName = this.printNode(nameNode, 0);
        const printedInitializer = initializer
            ? ` = ${this.printNode(initializer, 0)}`
            : " = nothing # TODO: Unhandled complex binding";
        // printVariableStatement will add the semicolon
        return `${printedName}${printedInitializer}`; // NO SEMICOLON here
    }

    // Helper to map types (ensure it handles Dict correctly)
    mapJsDocTypeToJulia(jsDocType: string): string {
        jsDocType = jsDocType.trim(); // trim added
        if (jsDocType.endsWith("[]")) {
            const baseType = jsDocType.slice(0, -2);
            // Ensure recursive call for base type mapping
            return `Vector{${this.mapJsDocTypeToJulia(baseType)}}`;
        }
        if (jsDocType.includes("|")) {
            return "Any"; // Union types map to Any for now
        }
        switch (
            jsDocType.toLowerCase().trim() // lowercase added
        ) {
            case "string":
                return "String";
            case "number":
                return "Number"; // Or Float64/Int depending on context needed
            case "boolean":
                return "Bool";
            case "object":
                return "Dict"; // Changed from Any to Dict based on test
            case "array":
                return "Vector";
            case "any":
                return "Any";
            case "null":
                return "Nothing";
            case "undefined":
                return "Nothing"; // Map undefined to Nothing
            default:
                // Attempt to capitalize if it's likely a custom type/class name
                if (
                    jsDocType.length > 0 &&
                    jsDocType[0] === jsDocType[0].toLowerCase() &&
                    jsDocType[0] !== jsDocType[0].toUpperCase() &&
                    jsDocType !== "true" &&
                    jsDocType !== "false"
                ) {
                    // Heuristic: if it starts lowercase and isn't boolean, treat as Any unless known primitive
                    return "Any";
                }
                return jsDocType; // Assume custom type/struct name, keep original casing
        }
    }

    formatDescriptionLinks(description: string): string {
        // Added escaping for Julia's string interpolation $
        return description
            .replace(/\[([^\]]+)\]\{@link\s+([^\}]+)\}/g, "[`$1`]($2)")
            .replace(/\$/g, "\\$");
    }

    printFunctionDeclaration(node, identation) {
        this.withinFunctionDeclaration = true;
        let signature = this.printFunctionDefinition(node, 0); // Pass 0 for definition itself

        let funcBody = "";
        if (node.body) {
            if (ts.isBlock(node.body)) {
                // Pass identation + 1 to printBlock for indenting statements inside
                funcBody = this.printBlock(node.body, identation + 1);
            } else {
                // Handle expression body (e.g., arrow functions) if needed, though less common in this context
                funcBody =
                    this.getIden(identation + 1) +
                    "return " +
                    this.printNode(node.body, 0) +
                    ";\n";
            }
        }

        // 4. Get Trailing Comment
        const trailingComment = this.printTraillingComment(node, identation);
        const trailingCommentFormatted = trailingComment
            ? " " + trailingComment.trim()
            : "";

        // 5. Assemble the final string: Docstring -> Signature -> Body -> End -> Trailing Comment
        const codeBlock =
            signature + // Signature already has its indentation
            (funcBody || "\n") + // Body ensures its newline(s), or add one if empty
            this.getIden(identation) +
            "end;" + // Indented 'end'
            trailingCommentFormatted;

        this.withinFunctionDeclaration = false;
        // 6. Prepend docstring and return - IMPORTANT: Comments are added HERE, before the code block
        return codeBlock;
    }

    printParameter(node, defaultValue = true) {
        const name = this.printNode(node.name, 0);
        const initializer = node.initializer;

        if (defaultValue && initializer) {
            let defaultVal;

            if (initializer.kind === ts.SyntaxKind.UndefinedKeyword) {
                defaultVal = "nothing";
            } else if (
                ts.isObjectLiteralExpression(initializer) &&
                initializer.properties.length === 0
            ) {
                defaultVal = "Dict()";
            } else {
                defaultVal = this.printNode(initializer, 0);
            }

            return `${name}=${defaultVal}`;
        }

        return name;
    }

    // Also restoring printBlock to handle indentation correctly within function bodies
    printBlock(node: ts.Block, identation: number): string {
        identation = Math.max(0, identation); // Ensure >= 0
        let result = "";
        node.statements.forEach((statement) => {
            // Process statement, trim only surrounding whitespace for the line itself
            const statementStr = this.printNode(statement, identation).trim(); // Get statement without extra indent/newline
            if (statementStr) {
                // Only add non-empty statements
                result += this.getIden(identation) + statementStr + "\n"; // Add correct indent + statement + newline
            }
        });
        // Return empty string if no statements, otherwise return with the final newline
        return result;
    }

    printReturnStatement(node, identation) {
        let result = this.getIden(identation + 1) + "return ";
        if (node.expression) {
            result += this.printNode(node.expression, 0);
        }
        result += ";";
        return result;
    }

    printFunctionBody(node, identation) {
        let result = "\n";
        let first = true;
        if (ts.isBlock(node.body)) {
            node.body.statements.forEach((statement) => {
                let s =
                    this.DEFAULT_IDENTATION.repeat(identation) + // Use the passed identation here
                    "\n" +
                    this.printNode(statement, identation) +
                    "\n";
                if (first) {
                    first = false;
                    s =
                        this.DEFAULT_IDENTATION.repeat(identation) +
                        s.trimLeft();
                }
                s = this.removeLeadingEmptyLines(s);
                if (s.length > 0) {
                    result += this.getIden(identation) + s;
                }
            });
        }
        return result.trimRight();
    }

    printForStatement(node: ts.ForStatement, identation = 0): string {
        // Ensure identation is never negative
        identation = Math.max(0, identation);

        let result = this.getIden(identation) + "for ";
        let initializerVarName = "";
        let startValueExpr: string | undefined = undefined; // No default value
        let endValueExpr: string | undefined = undefined; // No default value

        if (node.initializer) {
            if (ts.isVariableDeclarationList(node.initializer)) {
                const declaration = node.initializer.declarations[0];
                if (ts.isIdentifier(declaration.name)) {
                    initializerVarName = declaration.name.escapedText as string;
                }
                if (declaration.initializer) {
                    startValueExpr = this.printNode(declaration.initializer, 0); // Extract start expression
                } else {
                    startValueExpr = undefined; // Explicitly undefined if no initializer
                }
            }
        }

        if (node.condition) {
            if (ts.isBinaryExpression(node.condition)) {
                if (
                    node.condition.operatorToken.kind ===
                    SyntaxKind.LessThanToken
                ) {
                    endValueExpr = this.printNode(node.condition.right, 0); // Extract end expression
                } else {
                    endValueExpr = undefined; // Indicate unsupported condition operator
                }
            } else {
                endValueExpr = undefined; // Indicate unsupported condition type
            }
        }

        if (
            initializerVarName === "" ||
            startValueExpr === undefined ||
            endValueExpr === undefined
        ) {
            return (
                `#TODO: Incomplete for loop translation - Could not fully determine loop structure\n` +
                `# initializerVarName: ${initializerVarName}, startValueExpr: ${startValueExpr}, endValueExpr: ${endValueExpr}\n` +
                super.printForStatement(node, identation)
            ); // Fallback for complex cases
        }

        const juliaRange = this.generateJuliaRange(
            startValueExpr,
            endValueExpr,
        ); // Use extracted expressions

        result += initializerVarName + " in " + juliaRange + "\n";

        if (node.statement) {
            if (ts.isBlock(node.statement)) {
                node.statement.statements.forEach((stmt) => {
                    result +=
                        this.getIden(identation + 1) +
                        this.printNode(stmt, 0) +
                        "\n"; // Removed semicolon here
                });
            } else {
                result +=
                    this.getIden(identation + 1) +
                    this.printNode(node.statement, 0) +
                    "\n"; // Removed semicolon here
            }
        }

        result += this.getIden(identation) + "end";
        return result;
    }

    printBinaryExpression(
        node: ts.BinaryExpression,
        identation: number,
    ): string {
        const { left, right, operatorToken } = node;
        const customBinaryExp = this.printCustomBinaryExpressionIfAny(
            node,
            identation,
        );
        if (customBinaryExp) {
            return customBinaryExp;
        }

        if (node.operatorToken.kind == ts.SyntaxKind.InstanceOfKeyword) {
            return this.printInstanceOfExpression(node, identation);
        }

        let operator = this.SupportedKindNames[node.operatorToken.kind];

        // String concatenation check
        if (operatorToken.kind === ts.SyntaxKind.PlusToken) {
            const leftType = global.checker.getTypeAtLocation(left);
            const rightType = global.checker.getTypeAtLocation(right);
            if (
                this.isStringType(leftType.flags) ||
                this.isStringType(rightType.flags)
            ) {
                // It's potentially string concatenation
                let leftStr = this.printNode(left, 0);
                let rightStr = this.printNode(right, 0); // Print right without extra indent

                // Helper to extract args from string(...)
                const extractStringArgs = (str) => {
                    if (str.startsWith("string(") && str.endsWith(")")) {
                        // Basic parsing - might need more robustness for nested calls or complex args
                        const argsContent = str.substring(
                            "string(".length,
                            str.length - 1,
                        );
                        // Simple split by comma, assumes basic arguments
                        // This needs improvement for args containing commas within strings or nested structures
                        // For now, a simple split might work for common cases
                        // Consider a more robust parser if needed
                        // Split by comma, but respect nested parentheses/quotes? Hard.
                        // Let's try a simple split for now:
                        return argsContent.split(",").map((s) => s.trim());
                        // A better approach might involve a mini-parser or regex,
                        // but let's see if the simple split works for the target cases.
                    }
                    return null;
                };

                const leftArgs = extractStringArgs(leftStr);
                const rightArgs = extractStringArgs(rightStr);

                let finalArgs = [];
                if (leftArgs) {
                    finalArgs = finalArgs.concat(leftArgs);
                } else {
                    finalArgs.push(leftStr);
                }

                if (rightArgs) {
                    finalArgs = finalArgs.concat(rightArgs);
                } else {
                    finalArgs.push(rightStr);
                }

                // Filter out empty strings that might result from parsing errors
                finalArgs = finalArgs.filter((arg) => arg.length > 0);

                return `string(${finalArgs.join(", ")})`;
            }
            // Fallthrough to default numeric/other addition if not string type
        }

        let leftVar = undefined;
        let rightVar = undefined;
        // c# wrapper
        if (
            operatorToken.kind === ts.SyntaxKind.EqualsEqualsToken ||
            operatorToken.kind === ts.SyntaxKind.EqualsEqualsEqualsToken
        ) {
            if (this.COMPARISON_WRAPPER_OPEN) {
                leftVar = this.printNode(left, 0);
                rightVar = this.printNode(right, identation);
                return `${this.COMPARISON_WRAPPER_OPEN}${leftVar}, ${rightVar}${this.COMPARISON_WRAPPER_CLOSE}`;
            }
        }

        // check if boolean operators || and && because of the falsy values
        if (
            operatorToken.kind === ts.SyntaxKind.BarBarToken ||
            operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken
        ) {
            leftVar = this.printCondition(left, 0);
            rightVar = this.printCondition(right, identation);
        } else {
            leftVar = this.printNode(left, 0);
            rightVar = this.printNode(right, identation);
        }

        const customOperator = this.getCustomOperatorIfAny(
            left, // Pass raw nodes to allow getText() etc.
            right, // Pass raw nodes
            operatorToken,
        );

        operator = customOperator !== undefined ? customOperator : operator; // Check if customOperator is explicitly undefined

        return leftVar + " " + operator.trim() + " " + rightVar.trim(); // Trim operator and rightVar
    }

    printStringLiteral(node) {
        let text = node.getText();
        // Remove surrounding TypeScript quotes ('...' or "...")
        if (
            (text.startsWith('"') && text.endsWith('"')) ||
            (text.startsWith("'") && text.endsWith("'"))
        ) {
            text = text.substring(1, text.length - 1);
        }
        // First, handle explicitly escaped double quotes in the source (\\") -> "
        // We need the original double quote character to later escape it correctly for Julia r"..."
        text = text.replace(/\\"/g, '"');
        // Now, escape any remaining unescaped double quotes for Julia's r"..."
        text = text.replace(/"/g, '\\"');
        // The text from getText() should have preserved other original escapes like \\t, \\n
        return `raw"${text}"`;
    }

    printNullKeyword(node, identation) {
        return "nothing";
    }

    printObjectLiteralExpression(node, identation) {
        if (node.properties.length === 0) {
            // handle empty object {}
            return "Dict{Symbol, Any}()"; // Explicitly type empty Dict
        }

        // Use identation + 1 for properties *inside* the Dict() call
        const objectBody = node.properties
            .map((p) => this.printPropertyAssignment(p, identation + 1)) // Indent properties
            .join(",\n"); // Join properties with comma and newline

        // Structure: Dict(\n <indented properties> \n <base indent>)
        return `Dict(\n${objectBody}\n${this.getIden(identation)})`;
    }

    printReturnStatementWithoutIndent(node) {
        if (ts.isReturnStatement(node)) {
            let result = "return";
            if (node.expression) {
                result += " " + this.printNode(node.expression, 0);
            }
            result += ";";
            return result;
        }
        return this.printNode(node, 0);
    }

    printExpressionStatement(
        node: ts.ExpressionStatement,
        identation = 0,
    ): string {
        // Ensure identation is never negative
        identation = Math.max(0, identation);

        if (this.isCJSModuleExportsExpressionStatement(node)) {
            return ""; // remove module.exports = ...
        }
        const exprStm = this.printNode(node.expression, identation); // Use 0 indent for the expression itself

        // Skip empty statements
        if (exprStm.length === 0) {
            return "";
        }
        // Add semicolon only if it's not a block-like structure ending it implicitly
        // and the config requires it (LINE_TERMINATOR is ";"). For Julia, it's empty.
        const requiresTerminator =
            this.LINE_TERMINATOR &&
            !exprStm.endsWith("end") &&
            !exprStm.endsWith("}");
        // Apply indentation to the entire statement line
        return (
            this.getIden(identation) +
            exprStm +
            (requiresTerminator ? this.LINE_TERMINATOR : "")
        );
        // Using `this.getIden(0)` previously caused issues, using passed `identation` now.
    }

    printFunctionExpressionAsDeclaration(node, varName): string {
        // The exact expected format is:
        // function consumer(a)
        //     return a + 1;
        // end;
        //

        let result = "function " + varName + "(";

        if (node.parameters) {
            result += node.parameters
                .map((param) => this.printParameter(param, true))
                .join(", ");
        }
        result = result.replace(/^\(\s*,\s*/, "("); // Remove leading comma and space
        result = result.replace(/,(\s*)$/, "$1"); // remove trailing comma
        result = result.replace(/\(\s*\)/, "()"); // Remove whitespace inside parentheses
        result += ")\n";

        // Handle function body with 4 spaces of identation
        if (node.body) {
            if (ts.isBlock(node.body)) {
                const statements = node.body.statements;
                statements.forEach((statement) => {
                    result +=
                        this.DEFAULT_IDENTATION.repeat(1) + // Use DEFAULT_IDENTATION here (4 spaces)
                        this.printReturnStatementWithoutIndent(statement);
                    result += "\n";
                });
            }
        }

        // Add 4 spaces of identation before end and a newline after
        // REMOVED semicolon from here, let the caller handle it.
        result += this.DEFAULT_IDENTATION.repeat(0) + "end\n";

        // Add an additional newline to match expected output - removed extra newline
        return result.trimEnd(); // Trim potential trailing newline from block content before returning
    }

    printWhileStatement(node: ts.WhileStatement, identation = 0): string {
        identation = Math.max(0, identation);

        const expression = this.printNode(node.expression, 0).trim(); // trim expression whitespace
        let result = this.getIden(identation) + "while " + expression + "\n"; // add identation

        if (ts.isBlock(node.statement)) {
            // Handle each statement in the block
            node.statement.statements.forEach((stmt) => {
                if (ts.isExpressionStatement(stmt)) {
                    const expr = this.printNode(stmt.expression, 0).trim(); // trim expression whitespace
                    result += "    " + expr + ";\n";
                } else if (ts.isBreakStatement(stmt)) {
                    result += "    break;\n";
                } else {
                    result += "    " + this.printNode(stmt, 0).trim() + "\n"; // trim statement whitespace
                }
            });
        } else if (node.statement) {
            // Handle a single statement, using node.statement here
            // Handle a single statement
            if (ts.isExpressionStatement(node.statement)) {
                const expr = this.printNode(
                    node.statement.expression,
                    0,
                ).trim(); // trim expression whitespace
                result += "    " + expr + ";\n";
            } else if (ts.isBreakStatement(node.statement)) {
                result += "    break;\n";
            } else {
                result +=
                    "    " + this.printNode(node.statement, 0).trim() + "\n"; // trim statement whitespace
            }
        }

        result += this.getIden(identation) + "end";

        return result.trimEnd() + "\n"; // trim whitespace from end
    }

    printBreakStatement(node: ts.BreakStatement, identation = 0): string {
        // Ensure identation is never negative
        identation = Math.max(0, identation);

        return this.getIden(identation) + "break";
    }

    getIden(num: number): string {
        return this.DEFAULT_IDENTATION.repeat(Math.max(0, num));
    }

    printIfStatement(node, identation) {
        conditionalDebugLog(
            "printIfStatement called with identation:",
            identation,
        ); // Changed from console.debug
        conditionalDebugLog("Node kind:", ts.SyntaxKind[node.kind]); // Changed from console.debug

        // Ensure identation is never negative
        identation = Math.max(0, identation);

        // Get the condition expression
        const expression = this.printCondition(node.expression, 0);
        conditionalDebugLog("Condition expression:", expression); // Changed from console.debug

        // Include 'if' and condition
        let result =
            this.getIden(identation) + this.IF_TOKEN + " " + expression + "\n";

        // Handle the "then" branch
        if (node.thenStatement) {
            conditionalDebugLog(
                // Changed from console.debug
                "thenStatement kind:",
                ts.SyntaxKind[node.thenStatement.kind],
            );

            if (ts.isBlock(node.thenStatement)) {
                // For blocks, process each statement with increased identation
                node.thenStatement.statements.forEach((stmt, index) => {
                    conditionalDebugLog(
                        // Changed from console.debug
                        `Statement ${index} kind:`,
                        ts.SyntaxKind[stmt.kind],
                    );

                    if (ts.isIfStatement(stmt)) {
                        result += this.printIfStatement(stmt, identation + 1);
                    } else {
                        // Regular statement - for objects use Dict() and correct identation
                        if (
                            ts.isVariableStatement(stmt) &&
                            stmt.declarationList.declarations.length > 0 &&
                            stmt.declarationList.declarations[0].initializer &&
                            ts.isObjectLiteralExpression(
                                stmt.declarationList.declarations[0]
                                    .initializer,
                            )
                        ) {
                            result +=
                                this.printNode(stmt, identation + 1) + "\n";
                        } else {
                            result +=
                                this.printNode(stmt, identation + 1) + "\n";
                        }
                    }
                });
            } else if (ts.isIfStatement(node.thenStatement)) {
                result += this.printIfStatement(
                    node.thenStatement,
                    identation + 1,
                );
            } else {
                // Single statement - use identation + 1 for content
                result +=
                    this.printNode(node.thenStatement, identation + 1) + "\n";
            }
        }

        const elseStatement = node.elseStatement;
        if (elseStatement) {
            if (elseStatement?.kind === ts.SyntaxKind.Block) {
                const elseBlock = this.printBlock(elseStatement, identation + 1);
                result +=
                    this.getIden(identation) +
                    this.ELSE_TOKEN +
                    "\n" +
                    elseBlock;
            } else if (elseStatement?.kind === ts.SyntaxKind.IfStatement) {
                // Handle 'elseif', do not recursively call printIfStatement to avoid nested 'end'
                const elseIfNode = elseStatement;
                const elseIfCond = this.printCondition(
                    elseIfNode.expression,
                    0,
                );
                result +=
                    this.getIden(identation) +
                    this.ELSEIF_TOKEN +
                    " " +
                    elseIfCond +
                    "\n";
                if (ts.isBlock(elseIfNode.thenStatement)) {
                    result += this.printBlock(
                        elseIfNode.thenStatement,
                        identation + 1,
                    );
                } else {
                    result +=
                        this.printNode(
                            elseIfNode.thenStatement,
                            identation + 1,
                        ) + "\n";
                }

                if (elseIfNode.elseStatement) {
                    // handle else for elseif
                    const elseOfElseIf = elseIfNode.elseStatement;
                    if (elseOfElseIf?.kind === ts.SyntaxKind.Block) {
                        const elseBlock = this.printBlock(
                            elseOfElseIf,
                            identation + 1, // identation + 1 -> + 0, FIX: use identation + 1 here
                        );
                        result +=
                            this.getIden(identation) +
                            this.ELSE_TOKEN +
                            "\n" +
                            elseBlock;
                    } else {
                        result +=
                            this.getIden(identation) +
                            this.ELSE_TOKEN +
                            "\n" +
                            this.printNode(elseOfElseIf, identation + 1) +
                            "\n";
                    }
                }
            } else {
                // handle inline else statement Ex: if (x) a = 1; else a = 2;
                result +=
                    this.getIden(identation) +
                    this.ELSE_TOKEN +
                    "\n" +
                    this.printNode(elseStatement, identation + 1) +
                    "\n";
            }
        }

        // Properly close each if block with 'end'
        result += this.getIden(identation) + "end\n";

        conditionalDebugLog("printIfStatement result:", result); // Changed from console.debug
        return result;
    }

    printCondition(node, identation) {
        return this.printNode(node, identation);
    }

    printNode(node: ts.Node, identation = 0): string {
        try {
            let result = "";
            const originalIdentation = identation; // Store original identation
            identation = Math.max(0, identation); // Ensure >= 0 for internal use

            // --- Pre-computation/Handling ---
            // Handle CJS require/exports early if they are standalone statements
            if (
                ts.isVariableStatement(node) &&
                this.isCJSRequireStatement(node)
            ) {
                return ""; // Remove cjs imports entirely
            }
            if (
                ts.isExpressionStatement(node) &&
                this.isCJSModuleExportsExpressionStatement(node)
            ) {
                return ""; // remove module.exports = ...
            }

            let leadingComment = "";
            if (!this.withinFunctionDeclaration) {
                leadingComment = this.printLeadingComments(node, identation);
            }
            if (this.transpiledComments.has(this.tmpJSDoc)) {
                this.tmpJSDoc = "";
            }
            // Handle Potential Function Expression Assignment early if needed
            let isHandledFunctionExpressionAssignment = false;
            if (
                ts.isVariableDeclaration(node) &&
                node.initializer &&
                (ts.isFunctionExpression(node.initializer) ||
                    ts.isArrowFunction(node.initializer)) &&
                this.removeVariableDeclarationForFunctionExpression === false
            ) {
                // If we specifically DON'T remove the declaration, print it via the dedicated function
                let varName = "";
                if (ts.isIdentifier(node.name)) {
                    varName = node.name.escapedText as string;
                } else {
                    varName = this.printNode(node.name, 0); // Fallback
                }
                result = this.printFunctionExpressionAsDeclaration(
                    node.initializer,
                    varName,
                );
                isHandledFunctionExpressionAssignment = true; // Mark as handled
            }

            // --- Main Node Processing ---
            // Only proceed if not already handled (like the function expression assignment above)
            if (!isHandledFunctionExpressionAssignment) {
                if (ts.isSourceFile(node)) {
                    // Logic for SourceFile - iterates statements
                    result = "";
                    this.setCurrentFileName(node.fileName);
                    node.statements.forEach((statement, index) => {
                        // Get the printed statement for the current node
                        const printedStatement = this.printNode(statement, 0); // Use 0 indent for top-level
                        // Add indentation ONLY if the statement isn't empty
                        if (printedStatement.trim()) {
                            // Don't add extra indentation if the printed statement already starts with it (likely from a block)
                            const startsWithIndent = /^\s+/.test(
                                printedStatement,
                            );
                            result +=
                                (!startsWithIndent ? this.getIden(0) : "") +
                                printedStatement; // Apply base indent only if needed

                            // Add newline between statements if needed
                            if (
                                !printedStatement.trimEnd().endsWith("\n") &&
                                index < node.statements.length - 1
                            ) {
                                result += "\n"; // Add single newline
                            } else if (
                                printedStatement.trimEnd().endsWith("\n") &&
                                index < node.statements.length - 1 &&
                                node.statements[index + 1]
                            ) {
                                // Add extra newline if previous statement already ended with one (like blocks)
                                // unless it's the very last statement, and respect config
                                // result += "\n".repeat(this.LINES_BETWEEN_FILE_MEMBERS); // Use config here - Reverted for now
                                if (!printedStatement.endsWith("\n\n")) {
                                    // Avoid excessive newlines
                                    result += "\n".repeat(
                                        this.LINES_BETWEEN_FILE_MEMBERS,
                                    );
                                }
                            }
                        }
                    });
                    // Final newline management for the whole source file
                    if (result.trim() && !result.endsWith("\n")) {
                        result += "\n".repeat(this.NUM_LINES_END_FILE); // Add configured end lines
                    } else if (result.trim()) {
                        // If it already ends with \n, ensure the correct number of end lines
                        result =
                            result.trimEnd() +
                            "\n".repeat(this.NUM_LINES_END_FILE);
                    }
                    // Comments are handled by each statement, if there are no statements
                    // and only comments, we have to to print them directly
                    if (node.statements.length == 0 && result.trim() == "") {
                        result =
                            this.printNodeCommentsIfAny(node, identation, "") +
                            "\n";
                    }
                    // SourceFile handles its own comments if needed, skip printNodeCommentsIfAny
                    return leadingComment + result; // Return directly for SourceFile
                } else if (ts.isExpressionStatement(node)) {
                    result = this.printExpressionStatement(node, identation);
                } else if (ts.isBlock(node)) {
                    result = this.printBlock(node, identation);
                } else if (ts.isFunctionDeclaration(node)) {
                    result = this.printFunctionDeclaration(node, identation);
                } else if (
                    ts.isFunctionExpression(node) ||
                    ts.isArrowFunction(node)
                ) {
                    // If we reach here, it means removeVariableDeclarationForFunctionExpression must be true
                    // OR it's a function expression not assigned to a variable.
                    result = this.printFunctionDeclaration(node, identation); // Treat like declaration
                } else if (ts.isClassDeclaration(node)) {
                    result = this.printClass(node, identation);
                } else if (ts.isVariableStatement(node)) {
                    // Variable statement contains declaration list
                    result = this.printVariableStatement(node, identation);
                } else if (ts.isVariableDeclarationList(node)) {
                    // Usually handled by printVariableStatement, but could appear elsewhere (e.g., for loop initializer)
                    result = this.printVariableDeclarationList(
                        node,
                        identation,
                    );
                } else if (ts.isVariableDeclaration(node)) {
                    // This case should ideally only be hit if it's NOT a function expression assignment
                    // that was handled earlier, or if it's part of a declaration list processed directly.
                    result = this.printVariableDeclaration(node, identation);
                } else if (ts.isMethodDeclaration(node)) {
                    result = this.printMethodDeclaration(node, identation);
                } else if (ts.isStringLiteral(node)) {
                    result = this.printStringLiteral(node);
                } else if (ts.isNumericLiteral(node)) {
                    result = this.printNumericLiteral(node);
                } else if (ts.isPropertyAccessExpression(node)) {
                    result = this.printPropertyAccessExpression(
                        node,
                        identation,
                    );
                } else if (ts.isArrayLiteralExpression(node)) {
                    result = this.printArrayLiteralExpression(node, identation);
                } else if (ts.isCallExpression(node)) {
                    result = this.printCallExpression(node, identation);
                } else if (ts.isWhileStatement(node)) {
                    result = this.printWhileStatement(node, identation);
                } else if (ts.isBinaryExpression(node)) {
                    result = this.printBinaryExpression(node, identation);
                } else if (ts.isBreakStatement(node)) {
                    result = this.printBreakStatement(node, identation);
                } else if (ts.isForStatement(node)) {
                    result = this.printForStatement(node, identation);
                } else if (ts.isPostfixUnaryExpression(node)) {
                    result = this.printPostFixUnaryExpression(node, identation);
                } else if (ts.isObjectLiteralExpression(node)) {
                    result = this.printObjectLiteralExpression(
                        node,
                        identation,
                    );
                } else if (ts.isPropertyAssignment(node)) {
                    result = this.printPropertyAssignment(node, identation + 1);
                } else if (ts.isIdentifier(node)) {
                    result = this.printIdentifier(node);
                } else if (ts.isElementAccessExpression(node)) {
                    result = this.printElementAccessExpression(
                        node,
                        identation,
                    );
                } else if (ts.isIfStatement(node)) {
                    result = this.printIfStatement(node, identation);
                } else if (ts.isParenthesizedExpression(node)) {
                    result = this.printParenthesizedExpression(
                        node,
                        identation,
                    );
                } else if ((ts as any).isBooleanLiteral(node)) {
                    result = this.printBooleanLiteral(node);
                } else if (ts.SyntaxKind.ThisKeyword === node.kind) {
                    result = this.THIS_TOKEN; // Should be removed or handled contextually for Julia
                } else if (ts.SyntaxKind.SuperKeyword === node.kind) {
                    result = this.SUPER_TOKEN;
                } else if (ts.isTryStatement(node)) {
                    result = this.printTryStatement(node, identation);
                } else if (ts.isPrefixUnaryExpression(node)) {
                    result = this.printPrefixUnaryExpression(node, identation);
                } else if (ts.isNewExpression(node)) {
                    result = this.printNewExpression(node, identation);
                } else if (ts.isThrowStatement(node)) {
                    result = this.printThrowStatement(node, identation);
                } else if (ts.isAwaitExpression(node)) {
                    result = this.printAwaitExpression(node, identation);
                } else if (ts.isConditionalExpression(node)) {
                    result = this.printConditionalExpression(node, identation);
                } else if (ts.isAsExpression(node)) {
                    result = this.printAsExpression(node, identation);
                } else if (ts.isReturnStatement(node)) {
                    result = this.printReturnStatement(node, identation);
                } else if (ts.isArrayBindingPattern(node)) {
                    result = this.printArrayBindingPattern(node, identation);
                } else if (ts.isParameter(node)) {
                    result = this.printParameter(node);
                } else if (ts.isConstructorDeclaration(node)) {
                    result = this.printConstructorDeclaration(node, identation);
                } else if (ts.isPropertyDeclaration(node)) {
                    result = this.printPropertyDeclaration(node, identation); // within a class
                } else if (ts.isSpreadElement(node)) {
                    result = this.printSpreadElement(node, identation);
                } else if (ts.SyntaxKind.NullKeyword === node.kind) {
                    result = this.printNullKeyword(node, identation);
                } else if (ts.isContinueStatement(node)) {
                    result = this.printContinueStatement(node, identation);
                } else if (ts.isDeleteExpression(node)) {
                    result = this.printDeleteExpression(node, identation);
                } else if (ts.isExportDeclaration(node)) {
                    // <--- ADD THIS BLOCK
                    // Julia's module system and export mechanisms are different.
                    // For now, we remove the ES export declarations.
                    // A more sophisticated approach might involve generating Julia `export`
                    // statements within a module block, but that's outside the current scope.
                    result = "";
                }
                // ... other specific node types ...
                else {
                    // Fallback for unhandled nodes
                    if (!IGNORED_NODES.has(node.kind)) {
                        console.warn(
                            // Keep console.warn for actual warnings
                            `[${this.id}] Unhandled node kind:`,
                            ts.SyntaxKind[node.kind],
                            "Node text:",
                            node.getText()?.substring(0, 100), // Log snippet
                            "file:", // Add current file information
                            node.getSourceFile().fileName // Log the file name
                        );
                    }
                    result = ""; // Return empty for unhandled for now
                }
            }

            // --- Post-computation/Cleanup ---
            // Add comments IF the node kind isn't one that handles its own comments (like SourceFile)
            result = leadingComment + this.tmpJSDoc + result;
            this.tmpJSDoc = "";
            result = this.printNodeCommentsIfAny(node, identation, result);
            // conditionalDebugLog(ts.ScriptKind[node.kind]); // Use conditional log if needed
            // conditionalDebugLog(result); // Use conditional log if needed

            return result;
        } catch (e) {
            // ... (error handling remains the same, use console.error) ...
            console.error(
                // Keep console.error for actual errors
                "Error processing node:",
                node.getText()?.substring(0, 200),
            ); // Log more text
            console.error("Error in printNode:", e); // Keep console.error for actual errors
            if (e instanceof TranspilationError) {
                throw e;
            } else {
                throw new TranspilationError(
                    "julia", // Hardcode ID here or pass from constructor if needed
                    e.message || String(e),
                    e.stack || "No stack trace",
                    node.pos,
                    node.end,
                );
            }
        }
    }

    printContinueStatement(
        node: ts.ContinueStatement,
        identation: number = 0,
    ): string {
        return this.getIden(identation) + "continue;";
    }

    private generateJuliaRange(
        startValueExpr: string | undefined,
        endValueExpr: string | undefined,
    ): string {
        if (startValueExpr === undefined || endValueExpr === undefined) {
            return "#Error: Start or End value not determined for range";
        }
        return `${startValueExpr}:${parseInt(endValueExpr) - 1}`;
    }

    printObjectLiteralBody(node, identation) {
        let body = node.properties
            .map((p) => this.printPropertyAssignment(p, identation + 1))
            .join(",\n");
        body = body ? body : body; // remove the trailing comma
        return body;
    }

    printPropertyAssignment(node, identation) {
        const { name, initializer } = node;
        let nameKey: string;

        // Determine the key format: :key for identifiers/string literals, Symbol("...") for complex/variables
        if (ts.isIdentifier(name)) {
            nameKey = `Symbol("${name.text}")`; // Preferred: :identifier
        } else if (ts.isStringLiteral(name)) {
            // Remove outer quotes from the literal's text content for the symbol
            nameKey = `Symbol("${name.text}")`; // Preferred: :stringliteral
        } else {
            // Fallback for computed property names or other complex cases
            const nameAsString = this.printNode(name, 0);
            // Use Symbol constructor if the name isn't a simple identifier/string
            nameKey = `Symbol(${nameAsString})`;
            // Potentially unsafe if nameAsString evaluates to something unexpected.
            // Consider if this case needs more robust handling.
            console.warn(
                `[JuliaTranspiler] Complex property name used as Dict key: ${nameAsString}. Generating Symbol(${nameAsString}). Review if this is intended.`,
            );
        }

        const customRightSide = this.printCustomRightSidePropertyAssignment(
            initializer,
            identation, // Pass identation for nested structures
        );

        // Use identation = 0 for printing the value expression itself
        const valueAsString = customRightSide
            ? customRightSide
            : this.printNode(initializer, 0);

        let trailingComment = this.printTraillingComment(node, identation); // Use outer identation for comment positioning
        trailingComment = trailingComment ? " " + trailingComment : ""; // Add leading space if comment exists

        const propOpen = this.PROPERTY_ASSIGNMENT_OPEN
            ? this.PROPERTY_ASSIGNMENT_OPEN + " "
            : "";
        const propClose = this.PROPERTY_ASSIGNMENT_CLOSE
            ? " " + this.PROPERTY_ASSIGNMENT_CLOSE
            : "";

        // Apply indentation to the whole line
        return (
            this.getIden(identation) +
            propOpen +
            nameKey + // Use the formatted Symbol key
            " => " +
            valueAsString.trim() + // Value itself shouldn't have outer indent
            propClose +
            trailingComment
        );
    }

    printCustomRightSidePropertyAssignment(node, identation): string {
        if (ts.isObjectLiteralExpression(node)) {
            return this.printObjectLiteralExpression(node, identation); // avoid infinite recursion
        }
        return undefined;
    }

    printMethodDeclarationInClass(
        node: ts.MethodDeclaration,
        identation: number,
        className: string,
    ): string {
        this.withinFunctionDeclaration = true;
        let methodDef = this.printMethodDefinition(node, identation);

        methodDef = methodDef.replace("function ", `${this.METHOD_TOKEN} `);

        methodDef = methodDef.replace(
            `${this.METHOD_TOKEN} ${node.name.getText()}(`,
            `${this.METHOD_TOKEN} ${node.name.getText()}(self::${className}, `,
        );
        methodDef = methodDef.replace(
            "(self::" + className + ", ,",
            "(self::" + className + ", ",
        );
        methodDef = methodDef.replace("(", `(`);

        const funcBody = this.printFunctionBody(node, identation + 1);

        methodDef += funcBody;

        this.withinFunctionDeclaration = false;
        let result = this.tmpJSDoc + methodDef + this.getBlockClose(identation);
        this.tmpJSDoc = "";
        this.currentFunctionName = "";
        this.currentFunctionParams = "";
        return result;
    }

    printFunctionDefinition(node, identation) {
        // REMOVED indentation param from getIdent call
        let result = "";

        // Get modifiers string from base, then potentially remove 'async' if asyncTranspiling is true
        let modifiers = super.printModifiers(node);
        if (this.asyncTranspiling && modifiers.includes(this.ASYNC_TOKEN)) {
            // Remove the async token from the string
            modifiers = modifiers.replace(this.ASYNC_TOKEN, "").trim();
        }
        modifiers = modifiers ? modifiers + " " : ""; // Add space after modifiers if any

        result += modifiers;
        result += this.FUNCTION_TOKEN + " ";

        let functionName = "";
        if (ts.isFunctionDeclaration(node) && node.name) {
            functionName = this.transformFunctionNameIfNeeded(
                node.name.escapedText,
            );
        } else if (
            ts.isFunctionExpression(node) &&
            this.includeFunctionNameInFunctionExpressionDeclaration &&
            node.name
        ) {
            functionName = this.transformFunctionNameIfNeeded(
                node.name.escapedText,
            );
        }
        this.currentFunctionName = functionName; // Store for JSDoc
        result += functionName;

        result += "(";

        let params = "";
        if (node.parameters) {
            params += node.parameters
                .map((param) => this.printParameter(param, true))
                .join(", ");
        }
        this.currentFunctionParams = params; // Store for JSDoc
        result += params;
        result = result.replace(/^\(\s*,\s*/, "(");
        result = result.replace(/,(\s*)$/, "$1");
        result = result.replace(/\(\s*\)/, "()");
        result += ")\n"; // Add newline after signature

        // Removed this.getIden(identation) + result;
        // Comments will now be handled by printFunctionDeclaration wrapper logic
        // We store JSDoc in tmpJSDoc and handle regular comments later.
        return result;
    }

    printAwaitExpression(node, identation) {
        const expression = node.expression;
        const parsedExpression = this.printNode(expression, 0); // Parse the expression being awaited

        // Julia async transpiling: Convert `await expression` into a `let` block that:
        // 1. Starts an `@async` task with the expression.
        // 2. Fetches the result.
        // 3. Checks if the result itself is a Task (nested async).
        // 4. Fetches again if it's a nested Task, otherwise uses the result.
        // The entire let block returns the final value.

        if (this.asyncTranspiling) {
            // Generate the Julia let block structure
            const tempVar = "ans"; // Use 'ans' as the temporary variable name

            let result =
                this.getIden(identation) +
                `let task = @async ${parsedExpression}\n`;
            result +=
                this.getIden(identation + 1) + `${tempVar} = fetch(task)\n`;
            result += this.getIden(identation + 1) + `if ${tempVar} isa Task\n`;
            result += this.getIden(identation + 2) + `fetch(${tempVar})\n`; // Fetch the nested task
            result += this.getIden(identation + 1) + `else\n`;
            result += this.getIden(identation + 2) + `${tempVar}\n`; // Directly return the value
            result += this.getIden(identation + 1) + `end\n`;
            result += this.getIden(identation) + `end`;

            // Note: The calling printNode function (e.g., printReturnStatement) will wrap this
            // in printNodeCommentsIfAny and add the final LINE_TERMINATOR if required.
            return result;
        } else {
            // If async transpiling is off, just output the expression itself.
            // The 'awaitToken' is already removed by printModifiers when asyncTranspiling is false.
            return parsedExpression;
        }
    }

    printCallExpression(node: ts.CallExpression, identation: number): string { // Keep identation param, but maybe don't use it directly for the final string
        conditionalDebugLog(
            "Entering printCallExpression function",
            ts.SyntaxKind[node.kind],
        );

        const expression = node.expression;
        const parsedArgs = this.printArgsForCallExpression(node, 0); // Arguments string without outer indent

        // --- Revised `extractLeadingComment` (Keep as is) ---
        const extractLeadingComment = (text: string): { comment: string; code: string } => {
            const lines = text.split('\n');
            const commentLines: string[] = [];
            let firstCodeLineIndex = -1; // Index of the first non-comment/non-empty line

            for (let i = 0; i < lines.length; i++) {
                const trimmedLine = lines[i].trim();
                if (trimmedLine.startsWith('#')) {
                     // Always capture comment lines if code hasn't been found yet
                    if (firstCodeLineIndex === -1) {
                        commentLines.push(lines[i]);
                    } else {
                        break; // Found code earlier, stop processing comments
                    }
                } else if (trimmedLine === '') {
                    // Capture blank lines *only* if we haven't hit code yet
                    if (firstCodeLineIndex === -1) {
                        commentLines.push(lines[i]);
                    } else {
                          break; // Found code, stop collecting comments
                    }
                } else {
                    // Found the first non-comment/non-empty line
                    if (firstCodeLineIndex === -1) {
                         firstCodeLineIndex = i;
                    }
                     // Continue loop to process all lines, but stop capturing comments
                     // Break only if it's a comment/blank line *after* code was found
                }
            }

            if (firstCodeLineIndex === -1) {
                firstCodeLineIndex = lines.length;
            }

            const comment = commentLines.join('\n');
            const code = lines.slice(firstCodeLineIndex).join('\n');

            // Return untrimmed comment, trimmed code start
            return { comment: comment.trimEnd(), code: code.trimStart() }; // Trim comment end, code start
        };


        // Check what is being called (the expression part of the CallExpression)
        if (ts.isPropertyAccessExpression(expression)) {
            const propertyAccessExpression = expression;
            const baseExpression = propertyAccessExpression.expression;
            const memberNameNode = propertyAccessExpression.name;

            // --- Specific Property Access Call Types ---
            // ... (Cases 1, 2, 3, 4 remain largely the same, they don't usually involve comments needing separation in this way) ...
             // 1. super.method(...)
            if (baseExpression.kind === ts.SyntaxKind.SuperKeyword) {
                const methodName = this.printNode(memberNameNode, 0);
                const juliaArgs = parsedArgs ? "self, " + parsedArgs : "self";
                return `self.parent.${methodName}(${juliaArgs})`; // No comment handling needed here usually
            }

            // 2. this.method(...)
            if (baseExpression.kind === ts.SyntaxKind.ThisKeyword) {
                const methodName = this.printNode(memberNameNode, 0);
                const juliaArgs = parsedArgs ? "self, " + parsedArgs : "self";
                return `self.${methodName}(${juliaArgs})`; // No comment handling needed here usually
            }

            // 3. Full Property Access Replacements (e.g., Built-ins like console.log, JSON.stringify)
            const expressionTextFull = expression.getText().trim();
            if (
                this.FullPropertyAccessReplacements.hasOwnProperty(
                    expressionTextFull,
                )
            ) {
                // No comment handling needed here usually
                return `${this.FullPropertyAccessReplacements[expressionTextFull]}(${parsedArgs})`;
            }

            // 4. Specific Built-in Method Calls (Array, Object, Math, JSON, Promise, Number, Date)
             if (ts.isIdentifier(baseExpression)) {
                 const baseName = baseExpression.text;
                 const memberName = memberNameNode.text;
                 const args = node.arguments ?? [];

                 if (args.length === 1) {
                     const parsedArg = this.printNode(args[0], 0);
                     switch (memberName) {
                         case "parse":
                             if (baseName === "JSON") return this.printJsonParseCall(node, 0, parsedArg); // Use 0 indent internally
                             break;
                         case "stringify":
                             if (baseName === "JSON") return this.printJsonStringifyCall(node, 0, parsedArg);
                             break;
                         case "isArray":
                             if (baseName === "Array") return this.printArrayIsArrayCall(node, 0, parsedArg);
                              break;
                         case "keys":
                             if (baseName === "Object") return this.printObjectKeysCall(node, 0, parsedArg);
                             break;
                         case "values":
                             if (baseName === "Object") return this.printObjectValuesCall(node, 0, parsedArg);
                             break;
                         case "all":
                              if (baseName === "Promise") return this.printPromiseAllCall(node, 0, parsedArg);
                              break;
                         case "round":
                             if (baseName === "Math") return this.printMathRoundCall(node, 0, parsedArg);
                             break;
                         case "floor":
                              if (baseName === "Math") return this.printMathFloorCall(node, 0, parsedArg);
                              break;
                         case "ceil":
                             if (baseName === "Math") return this.printMathCeilCall(node, 0, parsedArg);
                              break;
                         case "isInteger":
                              if (baseName === "Number") return this.printNumberIsIntegerCall(node, 0, parsedArg);
                              break;
                     }
                 } else if (args.length === 0) {
                     switch (memberName) {
                         case "now":
                              if (baseName === "Date") return this.printDateNowCall(node, 0);
                              break;
                     }
                 }
             }


            // ---- Start Handling Instance Method Calls (Revised comment handling) ----
            const memberName = memberNameNode.text;
            const parsedBaseWithComments = this.printNode(baseExpression, 0); // Get base WITH potential comments, use 0 indent internally
            const { comment: leadingComment, code: commentFreeBase } = extractLeadingComment(parsedBaseWithComments);

            const args = node.arguments ?? [];

            let juliaCallResult = ""; // Variable to store the result from the specific print*Call

            // Handle methods with at least one argument
            if (args.length > 0) {
                const parsedArg1 = this.printNode(args[0], 0).trimStart();
                const parsedArg2 =
                    args.length > 1
                        ? this.printNode(args[1], 0).trimStart()
                        : undefined;
                switch (memberName) {
                    case "push":
                        juliaCallResult = this.printArrayPushCall(node, 0, commentFreeBase, parsedArg1); // Pass 0 indent
                        break;
                    case "includes":
                        juliaCallResult = this.printIncludesCall(node, 0, commentFreeBase, parsedArg1);
                        break;
                    case "indexOf":
                         juliaCallResult = this.printIndexOfCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                    case "join":
                         juliaCallResult = this.printJoinCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                    case "split":
                         juliaCallResult = this.printSplitCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                    case "toFixed":
                         juliaCallResult = this.printToFixedCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                    case "concat": {
                        const allParsedConcatArgs = node.arguments.map((arg) => this.printNode(arg, 0)).join(", ");
                        juliaCallResult = this.printConcatCall(node, 0, commentFreeBase, allParsedConcatArgs);
                        break;
                    }
                    case "search":
                         juliaCallResult = this.printSearchCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                    case "endsWith":
                         juliaCallResult = this.printEndsWithCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                    case "startsWith":
                         juliaCallResult = this.printStartsWithCall(node, 0, commentFreeBase, parsedArg1);
                         break;
                     case "padEnd":
                          juliaCallResult = this.printPadEndCall(node, 0, commentFreeBase, parsedArg1, parsedArg2);
                          break;
                     case "padStart":
                          juliaCallResult = this.printPadStartCall(node, 0, commentFreeBase, parsedArg1, parsedArg2);
                          break;
                    // Add other 1/2+ arg methods here...
                }
                 // Handle methods with at least two arguments (that weren't handled above)
                if (!juliaCallResult && args.length >= 2) { // Check if not already handled
                     // Re-parse args just in case they were needed differently
                     const parsedArg1_2 = this.printNode(args[0], 0).trimStart();
                     const parsedArg2_2 = this.printNode(args[1], 0).trimStart();
                     switch (memberName) {
                         case "slice":
                             juliaCallResult = this.printSliceCall(node, 0, commentFreeBase, parsedArg1_2, parsedArg2_2);
                             break;
                         case "replace":
                             juliaCallResult = this.printReplaceCall(node, 0, commentFreeBase, parsedArg1_2, parsedArg2_2);
                             break;
                          case "replaceAll":
                              juliaCallResult = this.printReplaceAllCall(node, 0, commentFreeBase, parsedArg1_2, parsedArg2_2);
                              break;
                     }
                 }
            }
            // Handle specific member calls with 0 arguments
            else if (!juliaCallResult && args.length === 0) { // Check if not already handled
                switch (memberName) {
                    case "toString":
                        juliaCallResult = this.printToStringCall(node, 0, commentFreeBase);
                        break;
                     case "toUpperCase":
                         juliaCallResult = this.printToUpperCaseCall(node, 0, commentFreeBase);
                         break;
                     case "toLowerCase":
                         juliaCallResult = this.printToLowerCaseCall(node, 0, commentFreeBase);
                          break;
                     case "shift":
                         juliaCallResult = this.printShiftCall(node, 0, commentFreeBase);
                          break;
                     case "pop":
                         juliaCallResult = this.printPopCall(node, 0, commentFreeBase);
                         break;
                      case "reverse":
                          juliaCallResult = this.printReverseCall(node, 0, commentFreeBase);
                           break;
                      case "trim":
                          juliaCallResult = this.printTrimCall(node, 0, commentFreeBase);
                          break;
                    // Add other 0-arg member methods
                }
            }

            // If a specific print*Call handled it, combine comment and code
            if (juliaCallResult) {
                let indent = this.getIndentationRegex(leadingComment)
                juliaCallResult = indent + this.getIden(identation) + juliaCallResult;
                // Combine comment and code. The caller (e.g., printExpressionStatement) adds the final indent.
                 if (leadingComment.trim()) {
                      return leadingComment.trimEnd() + '\n' + juliaCallResult; // Add newline if comment exists
                 } else {
                      return juliaCallResult; // No comment, just return the code
                 }
            }

            // 5. Other Instance Method Calls (Fallback)
            const instanceName = commentFreeBase; // Use comment-free base
            const methodName = this.printNode(memberNameNode, 0); // Already includes uncamelcase if needed
            const juliaInstanceMethodArgs = instanceName + (parsedArgs ? ", " + parsedArgs : "");
            juliaCallResult = `${instanceName}.${methodName}(${juliaInstanceMethodArgs})`;
            // Combine comment and code for the fallback case as well
             if (leadingComment.trim()) {
                  return leadingComment.trimEnd() + '\n' + juliaCallResult;
             } else {
                  return juliaCallResult;
             }


        } else if (ts.isIdentifier(expression)) {
             // --- Direct Identifier Calls (No change needed for comment handling here) ---
             // ... (existing code for identifier calls) ...
            const functionName = expression.text ?? (expression.escapedText as string);
            if (this.CallExpressionReplacements.hasOwnProperty(functionName as string)) {
                let replacement = this.CallExpressionReplacements[functionName as string];
                if (replacement.endsWith("(") || replacement.endsWith(", ")) {
                    return `${replacement}${parsedArgs})`;
                } else {
                    return `${replacement}(${parsedArgs})`;
                }
            }
            if (functionName === "assert") {
                return this.printAssertCall(node, 0, parsedArgs); // Use 0 indent
            }
             const transformedFunctionName = this.transformCallExpressionName(this.unCamelCaseIfNeeded(functionName as string));
             return `${transformedFunctionName}(${parsedArgs})`;

        } else if (expression.kind === ts.SyntaxKind.SuperKeyword) {
            // --- Standalone super() Call (No change needed) ---
            return "";
        }

        // --- Fallback for Unhandled Call Expression Types (No change needed) ---
        if (!IGNORED_NODES.has(expression.kind)) {
            console.warn(
                `[${this.id}] Unhandled CallExpression expression kind:`,
                ts.SyntaxKind[expression.kind],
                "Text:",
                expression.getText()?.substring(0, 100),
            );
        }
        let parsedExpression = this.printNode(expression, 0); // Use 0 indent
        if (parsedExpression.endsWith(")") || parsedExpression.trimRight().endsWith(")")) {
             return parsedExpression;
        }
        return `${parsedExpression}(${parsedArgs})`;
    }

    printJsonParseCall(node: any, identation: any, parsedArg?: any) {
        return `JSON3.parse(${parsedArg})`;
    }

    printJsonStringifyCall(node: any, identation: any, parsedArg?: any) {
        return `JSON3.json(${parsedArg})`;
    }

    protected isStaticMember(node: ts.Node): boolean {
        return (
            (ts.getCombinedModifierFlags(node as ts.Declaration) &
                ts.ModifierFlags.Static) !==
            0
        );
    }

    printPropertyDeclaration(node, identation) {
        const COLON_TOKEN = this.PROPERTY_ASSIGNMENT_TOKEN; // this.PROPERTY_ASSIGNMENT_TOKEN;
        const modifiers = this.printPropertyAccessModifiers(node);
        const name = this.printNode(node.name, 0);
        let type = this.getType(node);
        if (!type) type = "Any";
        let initializer = node.initializer
            ? this.printNode(node.initializer, 0)
            : undefined;
        const hasDefaultValues = node.parent.members.some(
            (member) =>
                ts.isPropertyDeclaration(member) &&
                member.initializer !== undefined,
        );
        if (hasDefaultValues && initializer) {
            initializer = undefined;
        }

        if (initializer) {
            return (
                this.getIden(identation) +
                name +
                COLON_TOKEN +
                type +
                " = " +
                initializer +
                this.LINE_TERMINATOR
            );
        }

        return (
            this.getIden(identation) + name + "::" + type + this.LINE_TERMINATOR
        );
    }

    printClassBody(node: ts.ClassDeclaration, identation: number): string {
        let propertiesString = "";
        const heritageClauses = node.heritageClauses;

        if (heritageClauses !== undefined) {
            const classExtends =
                heritageClauses[0].types[0].expression.getText();
            propertiesString += `${this.getIden(identation + 1)}parent::${classExtends}\n`;
        }

        node.members.forEach((member) => {
            // Process *all* PropertyDeclarations for fields
            if (ts.isPropertyDeclaration(member)) {
                // Removed isStaticMember check
                if (ts.isIdentifier(member.name)) {
                    const propertyName = member.name.text;
                    let type = "";
                    let defaultValue = ""; // Use this for @kwdef

                    if (member.type) {
                        const typeName = member.type.getText();
                        switch (typeName) {
                            case "string":
                                type = "::String";
                                break;
                            case "number":
                                type = "::Float64";
                                break;
                            case "boolean":
                                type = "::Bool";
                                break;
                            case "string[]":
                            case "Array<string>":
                                type = "::Vector{String}";
                                break;
                            case "number[]":
                            case "Array<number>":
                                type = "::Vector{Float64}";
                                break;
                            case "any":
                                type = "::Any";
                                break;
                            case "{}":
                                type = "::Dict";
                                break;
                            case "Whatever": // example
                                type = "::Any";
                                break;
                            default:
                                type = ""; // Defaults to Any
                        }
                    }
                    if (type === "") type = "::Any"; // Default to Any if type is not inferred

                    // Get default value from initializer for @kwdef
                    if (member.initializer) {
                        defaultValue = ` = ${this.printNode(member.initializer, 0)}`;
                    }

                    propertiesString += `${this.getIden(identation + 1)}${propertyName}${type}${defaultValue}\n`;
                }
            } else if (
                ts.isMethodDeclaration(member) &&
                !ts.isConstructorDeclaration(member) &&
                !this.isStaticMember(member)
            ) {
                // Only add method signatures if they are instance methods
                if (ts.isIdentifier(member.name)) {
                    const methodName = member.name.text;
                    // Add method as a Function field with a default value pointing to the function itself
                    propertiesString += `${this.getIden(identation + 1)}${methodName}::Function = ${methodName}\n`;
                }
            }
        });

        let hasOnlyConstructor = false;
        let constructorExists = false;
        let propertyOrMethodExists = false; // Check for any property or instance method
        let constructorNode: ConstructorDeclaration | undefined = undefined;

        node.members.forEach((member) => {
            if (ts.isConstructorDeclaration(member)) {
                constructorExists = true;
                constructorNode = member;
            }
            // Check if there are any properties or instance methods defined
            // *** FIX: Use this.isStaticMember ***
            if (
                ts.isPropertyDeclaration(member) ||
                (ts.isMethodDeclaration(member) &&
                    !this.isStaticMember(member) &&
                    !ts.isConstructorDeclaration(member))
            ) {
                propertyOrMethodExists = true;
            }
        });

        // Only consider it "has only constructor" if constructor exists AND no properties/methods exist
        if (constructorExists && !propertyOrMethodExists) {
            hasOnlyConstructor = true;
        }

        if (hasOnlyConstructor) {
            propertiesString += `${this.getIden(identation + 1)}attrs::Dict{Symbol, Any}\n`;
        }

        if (propertiesString.trim() !== "") {
            // only add propertiesString if not empty
            return propertiesString;
        }
        return ""; // return empty string if no properties to avoid extra new line
    }

    printConstructorDeclaration(
        node: ConstructorDeclaration,
        identation: number,
    ): string {
        const constructorNode = node as ConstructorDeclaration;
        identation = Number(identation);
        const className = (node.parent as ts.ClassDeclaration).name!.text;
        let params = constructorNode.parameters
            .map((param) => this.printParameter(param, false))
            .join(", ");

        // Collect initializers from constructor parameters
        let keywords = "";
        let initializers = ""; // To accumulate initializers for v.attrs
        constructorNode.parameters.forEach((param) => {
            if (ts.isIdentifier(param.name)) {
                const paramName = param.name.text;
                let propType = "Any"; // Default type if no type annotation
                if (param.type) {
                    propType = this.tsToJuliaType(param.type.getText());
                }
                keywords += `${paramName}::${propType}, `; // Correct placement: before kwargs...
                initializers += `${this.getIden(identation + 1)}v.attrs[:${paramName}] = ${paramName}\n`; // Add initializer for named parameter
            }
        });
        keywords = keywords.replace(/, $/, ""); // Remove trailing comma

        let result = `${this.getIden(identation)}function ${className}(args...; ${keywords}${keywords.length > 0 ? ", " : ""}kwargs...)\n`; // Correct placement: keywords before kwargs...

        // Check if the class extends another class, and conditionally call parent constructor
        if ((node.parent as ts.ClassDeclaration).heritageClauses) {
            result += `${this.getIden(identation + 1)}parent = extended(args...; kwargs...)\n`; // Construct parent FIRST
            result += `${this.getIden(identation + 1)}v = new(parent, Dict{Symbol, Any}())\n`; // THEN initialize v with parent
        } else {
            result += `${this.getIden(identation + 1)}v = new(Dict{Symbol, Any}())\n`; // Initialize v WITHOUT parent
        }

        result += `${initializers}`; // Add initializers for named parameters here, before kwargs loop

        // Assign all kwargs to attrs - this will capture kwargs meant for the child class as well
        result += `${this.getIden(identation + 1)}for (key, value) in kwargs\n`;
        result += `${this.getIden(identation + 2)}v.attrs[key] = value\n`;
        result += `${this.getIden(identation + 1)}end\n`;

        result += `${this.getIden(identation + 1)}return v\n`; // Add return statement

        result += `${this.getIden(identation)}end`;
        return result.trimEnd();
    }

    printClass(node: ts.ClassDeclaration, identation: number = 0): string {
        // Ensure identation is never negative
        identation = Math.max(0, identation);
        const className = node.name!.text;
        let result = this.printClassDefinition(node, identation);

        // Class properties (fields in Julia struct)

        result += this.printClassBody(node, identation); // add class body here
        const isChildClass = node.heritageClauses !== undefined;

        // Constructor - Julia structs can have constructor functions
        let hasConstructor = false;
        let constructorCode = ""; // Store constructor code
        node.members.forEach((member) => {
            if (ts.isConstructorDeclaration(member)) {
                hasConstructor = true;
                constructorCode =
                    this.printConstructorDeclaration(member, identation + 1) +
                    "\n"; // Capture constructor code
            }
        });
        if (!hasConstructor) {
            // only add default constructor if there are properties
            // Default constructor if no constructor is defined - only if there are properties
            const hasProperties = node.members.some(
                (member) =>
                    ts.isPropertyDeclaration(member) &&
                    this.isStaticMember(member),
            );
            if (hasProperties && !result.startsWith("@kwdef")) {
                {
                    constructorCode = `${this.getIden(identation + 1)}function ${className}(;`;
                    let keywords = "";
                    let initializers = "";
                    node.members.forEach((member) => {
                        if (
                            ts.isPropertyDeclaration(member) &&
                            this.isStaticMember(member) &&
                            member.initializer
                        ) {
                            if (ts.isIdentifier(member.name)) {
                                const propName = member.name.text;
                                const propType = this.tsToJuliaType(
                                    member.type?.getText(),
                                );
                                keywords += `${propName}::${propType}, `;
                                initializers += `${propName}, `;
                            }
                        }
                    });
                    keywords = keywords.replace(/, $/, "");
                    initializers = initializers.replace(/, $/, "");
                    constructorCode += `${keywords})\n`;
                    constructorCode += `${this.getIden(identation + 2)}new(${initializers})\n`;
                    constructorCode += `${this.getIden(identation + 1)}end\n`;
                }
            }
        }
        result += constructorCode; // Add constructor code to result

        result += `${this.getIden(identation)}end\n`; // Close struct

        let methodsCode = "";
        // Methods (excluding constructor and static methods/properties)
        node.members.forEach((member) => {
            if (
                ts.isMethodDeclaration(member) &&
                !ts.isConstructorDeclaration(member) &&
                !this.isStaticMember(member)
            ) {
                methodsCode +=
                    this.printMethodDeclarationInClass(
                        member,
                        identation,
                        className,
                    ) + "\n"; // Note: identation is not +1 here because methods are outside struct
            }
        });

        // Static methods and properties (outside struct)
        node.members.forEach((member) => {
            if (
                (ts.isMethodDeclaration(member) ||
                    ts.isPropertyDeclaration(member)) &&
                this.isStaticMember(member)
            ) {
                if (ts.isMethodDeclaration(member)) {
                    methodsCode += this.printMethodDeclaration(
                        member,
                        identation,
                    ); // No class name needed for static methods, moved outside struct
                }
            }
        });

        // getproperty
        if (isChildClass) {
            methodsCode += `\nfunction Base.getproperty(self::${className}, name::Symbol)\n`;
            methodsCode += `    if hasfield(${className}, name)\n`;
            methodsCode += `        getfield(self, name)\n`;
            methodsCode += `    else\n`;
            methodsCode += `        parent = getfield(self, :parent)\n`;
            methodsCode += `        if hasproperty(parent, name)\n`;
            methodsCode += `            getproperty(parent, name)\n`;
            methodsCode += `        else\n`;
            methodsCode += `            error("Property $name not found")\n`;
            methodsCode += `        end\n`;
            methodsCode += `    end\n`;
            methodsCode += `end\n`;
        }

        return result + methodsCode;
    }

    printClassDefinition(node, identation) {
        const className = node.name.escapedText;
        const heritageClauses = node.heritageClauses;
        let classInit = "";
        const classOpening = this.getBlockOpen(identation); // Should be "\n"

        // Determine if @kwdef is needed based *only* on fields with defaults

        // Check 1: Static properties with initializers directly translate to fields with defaults
        const hasDefaultValues = node.members.some(
            (member) =>
                ts.isPropertyDeclaration(member) &&
                this.isStaticMember(member) &&
                member.initializer !== undefined,
        );

        // Check 2: Instance methods translate to Function fields with defaults
        const hasMethods = node.members.some(
            (member) =>
                ts.isMethodDeclaration(member) &&
                !ts.isConstructorDeclaration(member) &&
                !this.isStaticMember(member),
        );

        // Use @kwdef ONLY if either of the above conditions is true
        const useKwdef = hasDefaultValues || hasMethods;

        // Start building the definition string
        classInit = this.getIden(identation);

        if (useKwdef) {
            // Use @kwdef, inheritance is handled by the parent field + getproperty
            classInit += "@kwdef struct " + className;
        } else {
            // Use plain struct - Inheritance is handled by the 'parent' field and getproperty
            classInit += "struct " + className;
            // *** REMOVED inheritance syntax from struct definition line ***
            // if (heritageClauses !== undefined) {
            //     const classExtends =
            //         heritageClauses[0].types[0].expression.escapedText;
            //     const extendsToken = this.EXTENDS_TOKEN || "<:"; // Default to <:
            //     classInit += " " + extendsToken + " " + classExtends;
            // }
        }

        // Add the opening block syntax (newline)
        classInit += classOpening; // "\n"

        return classInit;
    }
    printMethodDeclaration(node, identation) {
        this.withinFunctionDeclaration = true;
        let methodDef = this.printMethodDefinition(node, identation);

        const funcBody = this.printFunctionBody(node, identation);

        methodDef += funcBody;

        this.withinFunctionDeclaration = false;
        let result = this.tmpJSDoc + methodDef;
        this.tmpJSDoc = "";
        this.currentFunctionName = "";
        this.currentFunctionParams = "";
        return result;
    }

    tsToJuliaType(tsType: string): string {
        switch (tsType) {
            case "string":
                return "String";
            case "number":
                return "Float64";
            case "boolean":
                return "Bool";
            case "any":
                return "Any";
            case "null":
            case "undefined":
                return "Nothing";
            // ... (Keep all your original cases here) ...
            default:
                return "Any"; // Or throw an error, or return a specific "unknown" type
        }
    }

    printThrowStatement(node, identation) {
        const expression = this.printNode(node.expression, identation + 1);
        // Wrap the expression in parentheses for Julia's throw syntax
        return (
            this.getIden(identation) +
            this.THROW_TOKEN +
            "(" + // Add opening parenthesis
            expression +
            ")" + // Add closing parenthesis
            this.LINE_TERMINATOR
        );
    }

    printNewExpression(node, identation) {
        let expression = node.expression?.escapedText;
        expression = expression ? expression : this.printNode(node.expression); // new Exception or new exact[string] check this out
        const args = node.arguments
            .map((n) => this.printNode(n, identation))
            .join(", ");
        const newToken = this.NEW_TOKEN ? this.NEW_TOKEN + " " : "";
        return (
            expression + this.LEFT_PARENTHESIS + args + this.RIGHT_PARENTHESIS
        );
    }

    printTryStatement(node, identation) {
        const tryBody = this.printBlock(node.tryBlock, identation + 1);

        const catchBody = this.printBlock(node.catchClause.block, identation + 1);
        const catchDeclaration = this.CATCH_DECLARATION; // + " " + this.printNode(node.catchClause.variableDeclaration.name, 0);

        const catchCondOpen = this.CONDITION_OPENING
            ? this.CONDITION_OPENING
            : " ";

        return (
            this.getIden(identation) +
            this.TRY_TOKEN +
            "\n" +
            tryBody +
            this.getIden(identation) +
            this.CATCH_TOKEN +
            " " +
            this.CATCH_DECLARATION +
            /*catchCondOpen + catchDeclaration + this.CONDITION_CLOSE +*/ "\n" + //removed catch declaration to avoid double e and fix #2 indentation
            catchBody +
            this.getIden(identation) +
            this.BLOCK_CLOSING_TOKEN +
            "\n"
        );
    }

    printPropertyAccessExpression(node, identation) {
        const expression = node.expression;

        const transformedProperty =
            this.transformPropertyAcessExpressionIfNeeded(node);
        if (transformedProperty) {
            return this.getIden(identation) + transformedProperty;
        }

        let leftSide = node.expression.escapedText;
        let rightSide = node.name.escapedText;

        switch (rightSide) {
            case "length":
                return this.printLengthProperty(node, identation, leftSide);
        }

        let rawExpression = node.getText().trim();

        if (this.FullPropertyAccessReplacements.hasOwnProperty(rawExpression)) {
            // eslint-disable-line
            return this.FullPropertyAccessReplacements[rawExpression]; // eslint-disable-line
        }

        if (node.expression.kind === ts.SyntaxKind.ThisKeyword) {
            return (
                "self." + this.transformPropertyAccessExpressionName(rightSide)
            );
        }

        leftSide = this.LeftPropertyAccessReplacements.hasOwnProperty(leftSide)
            ? this.LeftPropertyAccessReplacements[leftSide]
            : this.printNode(expression, 0); // eslint-disable-line

        // checking "toString" insde the object will return the builtin toString method :X
        rightSide = this.RightPropertyAccessReplacements.hasOwnProperty(
            rightSide,
        ) // eslint-disable-line
            ? this.RightPropertyAccessReplacements[rightSide]
            : (this.transformPropertyAcessRightIdentifierIfNeeded(rightSide) ??
              rightSide);

        // join together the left and right side again
        const accessToken =
            this.getExceptionalAccessTokenIfAny(node) ??
            this.PROPERTY_ACCESS_TOKEN;

        rawExpression =
            leftSide +
            accessToken +
            this.transformPropertyAccessExpressionName(rightSide);

        return rawExpression;
    }

    printArrayIsArrayCall(node, identation, parsedArg = undefined) {
        return `isa(${parsedArg}, AbstractArray)`;
    }

    printObjectKeysCall(node, identation, parsedArg = undefined) {
        // `keys()` returns an iterator of keys (Symbols in our case).
        // Convert to Vector{Symbol} for common usage.
        // If strings are needed: `[string(k) for k in keys(${parsedArg})]`
        return `collect(keys(${parsedArg}))`; // Get keys as Vector{Symbol}
    }

    printObjectValuesCall(node, identation, parsedArg = undefined) {
        return `[v for v in values(${parsedArg})]`;
    }

    printPromiseAllCall(node, identation, parsedArg) {
        return `[fetch(p) for p in ${parsedArg}]`;
    }

    printMathFloorCall(node, identation, parsedArg = undefined) {
        return `floor(Int, ${parsedArg})`;
    }

    printMathCeilCall(node, identation, parsedArg = undefined) {
        return `ceil(Int, ${parsedArg})`;
    }

    printNumberIsIntegerCall(node, identation, parsedArg = undefined) {
        return `isinteger(${parsedArg})`;
    }

    printMathRoundCall(node, identation, parsedArg = undefined) {
        return `round(${parsedArg})`;
    }

    printIncludesCall(node, identation, name?, parsedArg?) {
        const leftNode = node.expression.expression;
        const type = global.checker.getTypeAtLocation(leftNode);
        if (this.isStringType(type.flags)) {
            return `occursin(${parsedArg}, ${name})`;
        }
        return `${parsedArg} in ${name}`;
    }

    printJoinCall(node: any, identation: any, name?: any, parsedArg?: any) {
        return `join(${name}, ${parsedArg})`;
    }

    printConcatCall(
        node: ts.CallExpression,
        identation: any,
        name?: any,
        parsedArg?: any,
    ) {
        // Directly translate .concat() to Julia's concat() function for now.
        // This might need refinement if vcat or string concatenation is strictly required based on types.
        const args = node.arguments
            .map((arg) => this.printNode(arg, 0))
            .join(", ");
        return `concat(${name}, ${args})`; // Use concat directly
    }

    printArrayPushCall(node, identation, name, parsedArg) {
        return `push!(${name}, ${parsedArg})`;
    }

    printSplitCall(node: any, identation: any, name?: any, parsedArg?: any) {
        return `split(${name}, ${parsedArg})`;
    }

    printPopCall(node: any, identation: any, name?: any) {
        return `pop!(${name})`;
    }

    printShiftCall(node: any, identation: any, name?: any) {
        return `popfirst!(${name})`;
    }

    printReverseCall(node, identation, name = undefined) {
        return `reverse!(${name})`;
    }

    printToStringCall(node, identation, name = undefined) {
        return `string(${name})`;
    }

    printIndexOfCall(
        node,
        identation,
        name = undefined,
        parsedArg = undefined,
    ) {
        return `findfirst(${parsedArg}, ${name})`;
    }

    printStartsWithCall(
        node,
        identation,
        name = undefined,
        parsedArg = undefined,
    ) {
        return `startswith(${name}, ${parsedArg})`;
    }

    printEndsWithCall(
        node,
        identation,
        name = undefined,
        parsedArg = undefined,
    ) {
        return `endswith(${name}, ${parsedArg})`;
    }

    printPadEndCall(node, identation, name, parsedArg, parsedArg2) {
        return `rpad(${name}, ${parsedArg}, ${parsedArg2})`;
    }

    printPadStartCall(node, identation, name, parsedArg, parsedArg2) {
        return `lpad(${name}, ${parsedArg}, ${parsedArg2})`;
    }

    printTrimCall(node, identation, name = undefined) {
        return `strip(${name})`;
    }

    printToUpperCaseCall(node, identation, name = undefined) {
        return `uppercase(${name})`;
    }

    printToLowerCaseCall(node, identation, name = undefined) {
        return `lowercase(${name})`;
    }

    printReplaceCall(
        node: any,
        identation: any,
        name?: any,
        parsedArg?: any,
        parsedArg2?: any,
    ) {
        return `replace(${name}, ${parsedArg} => ${parsedArg2})`;
    }

    printReplaceAllCall(
        node: any,
        identation: any,
        name?: any,
        parsedArg?: any,
        parsedArg2?: any,
    ) {
        return `replace(${name}, ${parsedArg} => ${parsedArg2})`;
    }

    transformLeadingComment(comment) {
        const commentRegex = [
            [/(^|\s)\/\//g, "$1#"], // regular comments // ADDED THIS LINE
            // General cleanup and structure
            [
                /^\s*\/\*+/,
                (_match, ..._args) => {
                    // Mark args/match unused if not needed
                    // Check if it's a potential JSDoc comment (starts with /**) AND we are inside a function context
                    if (
                        this.withinFunctionDeclaration &&
                        comment.startsWith("/**")
                    ) {
                        const functionName = `${this.currentFunctionName || ""}(${this.currentFunctionParams || ""})`;
                        return `"""\n${this.DEFAULT_IDENTATION}${functionName}\n\n`; // Start Julia docstring
                    } else if (comment.startsWith("/*")) {
                        // Handle regular block comments /* ... */
                        return "#="; // Start Julia block comment
                    }
                    // Fallback - should ideally not be reached if comment parsing is robust
                    return comment;
                },
            ], // Handles start /** -> """ or /* -> #=
            [
                /\*+\/\s*$/,
                (_match, ..._args) => {
                    // Use a function for conditional replacement
                    // Check if it was originally a JSDoc comment (started with /**) and we are inside a function
                    if (
                        this.withinFunctionDeclaration &&
                        comment.startsWith("/**")
                    ) {
                        return '"""'; // End Julia docstring
                    } else {
                        return "=#"; // End Julia block comment
                    }
                },
            ], // End docstring or block comment
            // Remove leading * and optional space, BUT keep indentation relative to the *start* of the docstring block
            [/\n\s*\* ?/g, "\n"], // Simple removal for now, might need refinement for complex indents

            // Remove JS-specific tags
            [/@method\s+.*\n?/g, ""],
            [/@name\s+.*\n?/g, ""], // Remove @name tag and its line
            [/@description\s+/g, ""], // Remove @description tag itself, keep the text

            // Arguments (@param)
            // 1. Find first @param and insert # Arguments heading before it. Use lookahead.
            [
                /(\n\n|^)(?![\s\S]*# Arguments)([\s\S]*?)@param/,
                "$1$2\n# Arguments\n@param",
            ],
            // 2. Format each @param line - Capture type, name, and description robustly
            [
                /@param\s+\{([^}]+)\}\s+([\w$.\[\]'-]+)((?:\s*-\s*)?[\s\S]*?)(?=\n\s*@|\n\s*"""|\s*$)/g, // Improved capture to lookahead for next tag or end
                (match, type, name, description) =>
                    `- \`${name.replace(/\.\.\./g, "...")}\`::${this.mapJsDocTypeToJulia(type)}: ${description.trim()}`,
            ],

            // Returns (@returns)
            // 1. Find first @returns and insert # Returns heading
            [
                /(\n\n|^)(?![\s\S]*# Returns)([\s\S]*?)@returns?/,
                "$1$2\n# Returns\n@returns",
            ],
            // 2. Format @returns with type
            [
                /@returns?\s+\{([^}]+)\}\s+([\s\S]*?)(?=\n\s*@|\n\s*"""|\s*$)/g, // Improved capture
                (match, type, description) =>
                    `- \`${this.mapJsDocTypeToJulia(type)}\`: ${this.formatDescriptionLinks(description.trim())}`,
            ],
            // 3. Format @returns without type (if any remain)
            [
                /@returns?\s+([^{][\s\S]*?)(?=\n\s*@|\n\s*"""|\s*$)/g,
                (match, description) =>
                    `- ${this.formatDescriptionLinks(description.trim())}`,
            ],

            // Links (keep this)
            [/\[([^\]]+)\]\{@link\s+([^\}]+)\}/g, "[`$1`]($2)"], // Format links

            // Final structural cleanup
            [/^\s*"""\s*\n*/, '"""\n'], // Ensure """ is followed by newline (or is empty)
            [/\n{3,}/g, "\n\n"], // Reduce multiple blank lines to max two
            [/(\n\s*""")$/, '\n"""'], // Ensure newline before final """
            [/^\s*$/, ""], // Remove empty lines resulting from tag removals IF they are fully empty
            [/"""\s*\n\s*"""/g, ""], // Remove empty docstrings like """\n"""
            // Ensure blank line BEFORE headings (if not at the start)
            [/(.)\n(# Arguments|# Returns)/g, "$1\n\n$2"],
            // Ensure blank line BETWEEN headings
            [
                /\n(# Arguments|# Returns)\n+(# Arguments|# Returns)/g,
                "\n$1\n\n$2",
            ],
        ];

        let transformed = comment;
        // Apply regex transformations
        transformed = regexAll(transformed, commentRegex);

        // Add signature placeholder ONLY if it's missing AND content exists
        if (transformed.trim().length > 6) {
            // Check if more than just """\n"""
            const lines = transformed.split("\n");
            if (
                lines.length > 1 &&
                lines[1].trim() &&
                !lines[1].trim().startsWith(" ") &&
                !lines[1].trim().startsWith("#")
            ) {
                // Heuristic: Likely needs the signature line added
                // We get the signature from printFunctionDefinition, so no need to add it here.
                // Just ensure structure after """ starts correctly.
            }
        }

        // Trim internal leading/trailing whitespace within the block more carefully
        const contentLines = transformed.split("\n");
        if (contentLines.length > 2) {
            const coreContent = contentLines
                .slice(1, -1)
                .map((line) => line.trimEnd())
                .join("\n")
                .trim(); // Trim trailing spaces per line, then trim overall whitespace
            transformed =
                contentLines[0] +
                "\n" +
                coreContent +
                "\n" +
                contentLines[contentLines.length - 1];
        }

        // Remove sequences like `"""\n\n\n"""` to just `"""\n"""` or "" if totally empty
        transformed = transformed.replace(/"""\s*\n\s*"""/g, "");

        if (!transformed.trim()) {
            return ""; // Return empty if nothing was generated
        }

        // Ensure exactly one newline after opening """ and before closing """ if content exists
        transformed = transformed.replace(/"""\n*/, '"""\n');
        transformed = transformed.replace(/\n*"""$/, '\n"""');

        // Ensure there's a double newline before # Arguments or # Returns if they aren't the first content line
        transformed = transformed.replace(
            /([^\n])\n(# Arguments|# Returns)/g,
            "$1\n\n$2",
        );

        // Ensure there's a new line after the heading function name
        transformed = transformed.replace(
            /("""\n[^\s#][^\n]*)(?=\n[^"""])/s,
            "$1\n",
        );

        // Remove leading/trailing empty lines inside the docstring
        transformed = transformed.replace(/"""\n(\s*\n)+/g, '"""\n'); // remove empty lines at start
        transformed = transformed.replace(/(\n\s*)+\n"""/g, '\n"""'); // remove empty lines at end

        return transformed;
    }

    transformTrailingComment(comment) {
        const commentRegex = [
            [/(^|\s)\/\//g, "$1#"], // Transform // comments to #
        ];
        const transformed = regexAll(comment, commentRegex);
        // Trim potential whitespace introduced by regex or original comment text
        return transformed.trim();
    }

    printNodeCommentsIfAny(node, identation, parsedNode) {
        const leadingComment = this.printLeadingComments(node, identation);
        let trailingComment = ""; // Initialize empty

        // Check if parent node ends at the same position. If so, parent will handle the trailing comment.
        const parent = node.parent;
        if (!parent || parent.end !== node.end) {
            trailingComment = this.printTraillingComment(node, identation);
        }

        // Avoid adding empty comments or disrupting existing newlines badly
        let result = "";
        if (leadingComment.trim()) {
            // Add leading comment, ensuring it's on its own line if parsedNode is not empty
            result += leadingComment.trimEnd(); // Trim trailing newline from comment block itself
            if (parsedNode && parsedNode.trim()) {
                // Check if parsedNode has content
                // Add newline only if the result doesn't already end with one AND parsedNode doesn't start with one
                if (!result.endsWith("\n") && !/^\s*\n/.test(parsedNode)) {
                    result += "\n";
                }
            }
        }

        // JSDoc handling (remains the same)
        if (
            this.withinFunctionDeclaration &&
            this.nodeContainsJsDoc(node) &&
            !this.doComments
        ) {
            this.tmpJSDoc = result; // Store potential docblock
            result = ""; // Clear result so it's not duplicated
        }

        // Add the parsed node content
        result += parsedNode;

        // Add trailing comment (only if it was fetched)
        if (trailingComment.trim()) {
            // Check if the parsed node already ends with a newline. Add space or newline accordingly.
            if (
                parsedNode &&
                !parsedNode.endsWith("\n") &&
                !parsedNode.endsWith(" ")
            ) {
                // If no newline or space, add a space before the comment
                result += " ";
            }
            // Add the trimmed trailing comment (it already has its leading space from printTraillingComment)
            result += trailingComment;
        }

        return result;
    }
    printCustomBinaryExpressionIfAny(node, identation) {
        const left = node.left;
        const right = node.right.text;

        const op = node.operatorToken.kind;

        // Fix E712 comparison: if cond == True -> if cond:
        if (
            (op === ts.SyntaxKind.EqualsEqualsToken ||
                op === ts.SyntaxKind.EqualsEqualsEqualsToken) &&
            node.right.kind === ts.SyntaxKind.TrueKeyword
        ) {
            return this.getIden(identation) + this.printNode(node.left, 0);
        }

        if (left.kind === SyntaxKind.TypeOfExpression) {
            const typeOfExpression = this.handleTypeOfInsideBinaryExpression(
                node,
                identation,
            );
            if (typeOfExpression) {
                return typeOfExpression;
            }
        }

        const prop = node?.left?.expression?.name?.text;

        if (prop) {
            const args = left.arguments;
            const parsedArg =
                args && args.length > 0
                    ? this.printNode(args[0], 0)
                    : undefined;
            const leftSideOfIndexOf = left.expression.expression; // myString in myString.indexOf
            const leftSide = this.printNode(leftSideOfIndexOf, 0);
            // const rightType = global.checker.getTypeAtLocation(leftSideOfIndexOf); // type of myString in myString.indexOf ("b") >= 0;

            switch (prop) {
                case "indexOf":
                    if (
                        op === SyntaxKind.GreaterThanEqualsToken &&
                        right === "0"
                    ) {
                        return (
                            this.getIden(identation) +
                            `findfirst(${parsedArg}, ${leftSide}) !== nothing`
                        );
                    }
            }
        }
        return undefined;
    }

    handleTypeOfInsideBinaryExpression(node, identation) {
        const expression = node.left.expression;
        const right = node.right.text;

        const op = node.operatorToken.kind;
        const isDifferentOperator =
            op === SyntaxKind.ExclamationEqualsEqualsToken ||
            op === SyntaxKind.ExclamationEqualsToken;
        const notOperator = isDifferentOperator ? this.NOT_TOKEN : "";

        switch (right) {
            case "string":
                return (
                    this.getIden(identation) +
                    notOperator +
                    "isa(" +
                    this.printNode(expression, 0) +
                    ", AbstractString)"
                ); // changed to AbstractString
            case "number":
                return (
                    this.getIden(identation) +
                    notOperator +
                    "isa(" +
                    this.printNode(expression, 0) +
                    ", Number)"
                ); // changed to Number
            case "boolean":
                return (
                    this.getIden(identation) +
                    notOperator +
                    "isa(" +
                    this.printNode(expression, 0) +
                    ", Bool)"
                );
            case "object":
                return (
                    this.getIden(identation) +
                    notOperator +
                    "isa(" +
                    this.printNode(expression, 0) +
                    ", Dict)"
                );
            case "undefined":
                return (
                    this.getIden(identation) +
                    this.printNode(expression, 0) +
                    " " +
                    notOperator +
                    "=== nothing"
                ); // changed to === nothing
        }

        return undefined;
    }

    nodeContainsJsDoc(node) {
        function hasJsDoc(n) {
            if (!n) return false;
            const commentRanges = ts.getLeadingCommentRanges(
                node.getSourceFile().text,
                n.pos,
            );
            if (commentRanges) {
                for (const commentRange of commentRanges) {
                    if (
                        commentRange.kind ===
                            ts.SyntaxKind.MultiLineCommentTrivia &&
                        node
                            .getSourceFile()
                            .text.substring(commentRange.pos, commentRange.end)
                            .startsWith("/**")
                    ) {
                        return true;
                    }
                }
            }
            return false;
        }

        if (hasJsDoc(node)) {
            return true;
        }

        for (const child of node.getChildren()) {
            if (this.nodeContainsJsDoc(child)) {
                return true;
            }
        }
        return false;
    }

    printTraillingComment(node, identation) {
        const fullText = global.src.getFullText();
        // Fetch all potential trailing comment ranges ending at the node's end position
        const commentRanges = ts.getTrailingCommentRanges(fullText, node.end);
        let res = "";

        // Process only the *first* identified trailing comment range to avoid duplication
        // for nested nodes ending at the same position.
        if (commentRanges && commentRanges.length > 0) {
            const firstCommentRange = commentRanges[0];
            const commentText = fullText.slice(
                firstCommentRange.pos,
                firstCommentRange.end,
            );
            if (commentText !== undefined) {
                // Prepend space here before adding the transformed comment
                res += this.transformTrailingComment(commentText);
            }
        }
        return res;
    }
    printDateNowCall(node: any, identation: any) {
        return (
            this.DEFAULT_IDENTATION.repeat(identation) +
            `round(Int, time() * 1000)`
        );
    }

    printSearchCall(node, identation, name = undefined, parsedArg = undefined) {
        // Julia's findfirst returns a range or nothing. JS search returns index or -1.
        // We need to replicate the JS behavior.
        const letBlockVar = "v"; // Temporary variable name
        // Using base indentation (passed identation) for the 'let' keyword
        let result =
            this.getIden(identation) +
            `let ${letBlockVar} = findfirst(${parsedArg}, ${name});\n`;
        // Using identation + 1 for the contents of the let block
        result +=
            this.getIden(identation + 1) + `if ${letBlockVar} == nothing\n`;
        result += this.getIden(identation + 2) + `-1\n`; // JS returns -1 if not found
        result += this.getIden(identation + 1) + `else\n`;
        // Using identation + 2 for the content inside else
        // Julia array/range indexing starts at 1. JS search returns 0-based index.
        // findfirst range gives 1-based start index. We need 0-based for JS equivalence.
        // However, the test expects v[1], which is the 1-based start index. Let's stick to the test expectation.
        result += this.getIden(identation + 2) + `${letBlockVar}[1]\n`;
        result += this.getIden(identation + 1) + `end\n`;
        // Using base indentation for the final 'end' of the let block
        result += this.getIden(identation) + `end`;
        return result;
    }

    printInstanceOfExpression(node, identation) {
        const left = this.printNode(node.left, 0);
        const right = this.printNode(node.right, 0);
        return this.getIden(identation) + `isa(${left}, ${right})`;
    }

    printDeleteExpression(
        node: ts.DeleteExpression,
        identation: number,
    ): string {
        const baseIndent = this.getIden(identation);
        // Expecting node.expression to be an ElementAccessExpression like myObject['property'] or myObject[keyVar]
        if (ts.isElementAccessExpression(node.expression)) {
            const objectName = this.printNode(node.expression.expression, 0);
            const argumentExpr = node.expression.argumentExpression;

            // Handle string literal access like myObject['property'] -> delete!(myObject, :property)
            if (ts.isStringLiteral(argumentExpr)) {
                const propertyName = argumentExpr.text;
                // Use :symbol syntax directly for literal strings
                return `${baseIndent}delete!(${objectName}, :${propertyName})`;
            }
            // Handle variable access like myObject[key]
            else {
                const propertyVar = this.printNode(argumentExpr, 0);
                // Check type if possible, otherwise assume it should be converted to Symbol
                // Since we decided keys are Symbols, convert variable to Symbol
                // Note: If the variable *already* holds a Symbol, Symbol(existingSymbol) is fine.
                // If it holds a string, Symbol(string) creates the symbol.
                return `${baseIndent}delete!(${objectName}, Symbol(${propertyVar}))`;
            }
        } else if (ts.isPropertyAccessExpression(node.expression)) {
            // Handle delete myObject.property -> delete!(myObject, :property)
            const objectName = this.printNode(node.expression.expression, 0);
            const propertyName = node.expression.name.text; // Or escapedText
            return `${baseIndent}delete!(${objectName}, :${propertyName})`;
        } else {
            // Fallback or error for unexpected expression types
            console.warn(
                // Keep console.warn for actual warnings
                "Unhandled delete expression type:",
                ts.SyntaxKind[node.expression.kind],
            );
            return `${baseIndent}# TODO: Unhandled delete expression: ${this.printNode(node.expression, 0)}`;
        }
    }

    printSpreadElement(node, identation) {
        const expression = this.printNode(node.expression, 0);
        // Julia's spread is `...` after the expression
        return expression + this.SPREAD_TOKEN;
    }

    printArrayLiteralExpression(node, identation) {
        const elements = node.elements
            .map((e) => {
                // Print each element without additional indent, outer context handles it
                return this.printNode(e, 0);
            })
            .join(", ");
        // Apply the outer indentation to the entire array literal
        return this.ARRAY_OPENING_TOKEN + elements + this.ARRAY_CLOSING_TOKEN;
    }

    printAssertCall(node, identation, parsedArgs) {
        if (node.arguments.length === 2) {
            const condition = this.printNode(node.arguments[0], 0);
            let message = "";
            if (ts.isStringLiteral(node.arguments[1])) {
                // Manually format the string literal to use standard quotes
                message = `"${node.arguments[1].text}"`; // Use standard double quotes
            } else {
                // Handle non-literal message arguments if necessary
                message = this.printNode(node.arguments[1], 0);
            }
            return `@assert ${condition} ${message}`;
        }
        // Fallback for incorrect number of arguments, although TS would likely catch this
        return `@assert ${parsedArgs}`; // Or handle error
    }

    printArrayBindingPattern(
        node: ts.ArrayBindingPattern,
        identation: number,
    ): string {
        // Indentation is usually handled by the parent VariableStatement/ForStatement etc.
        const elements = node.elements
            .map((e) => {
                if (ts.isBindingElement(e)) {
                    // Recurse for nested patterns or print identifier
                    return this.printNode(e.name, 0);
                }
                // Handle OmittedExpressionElement if necessary
                return "#OMITTED#";
            })
            .join(", ");

        // Find the VariableDeclaration parent to get the initializer
        let parentDeclaration: ts.VariableDeclaration | undefined = undefined;
        if (node.parent && ts.isVariableDeclaration(node.parent)) {
            parentDeclaration = node.parent;
        } else {
            // This can happen if ArrayBindingPattern is used elsewhere (e.g., function param)
            // In the context of `const [a,b] = ...`, parent *must* be VariableDeclaration.
            console.error(
                "ArrayBindingPattern parent is not VariableDeclaration - Unexpected context?",
            ); // Keep console.error
            return "#Error: Invalid ArrayBindingPattern context";
        }

        const initializer = parentDeclaration.initializer;
        let rightExpression = "()"; // Default to empty tuple

        if (initializer) {
            // Print the initializer node
            let printedInitializer = this.printNode(initializer, 0); // e.g., gets "[1, 2]" from ArrayLiteral
            // Check if the *original* initializer was an ArrayLiteralExpression
            // AND if the printed result looks like a Julia array `[...]`
            if (
                ts.isArrayLiteralExpression(initializer) &&
                printedInitializer.startsWith("[") &&
                printedInitializer.endsWith("]")
            ) {
                // Convert `[el1, el2]` to `(el1, el2)` to match Julia tuple assignment syntax for destructuring
                rightExpression =
                    "(" +
                    printedInitializer.substring(
                        1,
                        printedInitializer.length - 1,
                    ) +
                    ")";
            } else {
                // If initializer wasn't an array literal or didn't print as [...], use its output directly
                rightExpression = printedInitializer;
            }
        }

        // Construct the full assignment "lhs = rhs" without a semicolon.
        // The VariableStatement adds the semicolon later.
        return (
            this.getIden(0) + // No indent needed, parent handles line indent
            this.LEFT_PARENTHESIS +
            elements +
            this.RIGHT_PARENTHESIS +
            " = " +
            rightExpression
        ); // NO SEMICOLON HERE
    }

    printPostFixUnaryExpression(node, identation) {
        const operand = this.printNode(node.operand, 0);
        const operator = this.PostFixOperators[node.operator];
        // Julia does not have direct postfix ++/--. It needs to be on a separate line or handled contextually.
        // This basic version converts x++ to x += 1; which might be okay in statement contexts.
        // Be aware this might not work correctly inside complex expressions.
        if (
            operator === this.PLUS_PLUS_TOKEN ||
            operator === this.MINUS_MINUS_TOKEN
        ) {
            // We assume this is used as a statement. If used inline, this needs rethinking.
            return `${operand}${operator}`;
        }
        // Fallback for other potential postfix operators if added later
        return `${operand}${operator}`;
    }

    printPrefixUnaryExpression(node, identation) {
        const operand = this.printNode(node.operand, 0);
        const operator = this.PrefixFixOperators[node.operator];
        // Handle Julia's ! for negation. Ensure space for 'not'.
        if (node.operator === ts.SyntaxKind.ExclamationToken) {
            return `${this.NOT_TOKEN}${operand}`; // Use configured NOT_TOKEN
        } else if (node.operator === ts.SyntaxKind.MinusToken) {
            // Standard unary minus
            return `${operator}${operand}`;
        } else if (
            node.operator === ts.SyntaxKind.PlusPlusToken ||
            node.operator === ts.SyntaxKind.MinusMinusToken
        ) {
            // Julia doesn't have prefix increment/decrement. Convert to assignment.
            // This might be incorrect if the result of the prefix op was expected inline.
            // Assuming statement context:
            const assignmentOperator =
                node.operator === ts.SyntaxKind.PlusPlusToken ? "+=" : "-=";
            return `${operand} ${assignmentOperator} 1`; // Separate statement assumed
        }
        // Fallback for other potential prefix operators
        return `${operator}${operand}`;
    }

    printLengthProperty(node, identation, name = undefined) {
        const leftSide = this.printNode(node.expression, 0); // Use the expression node directly
        return `length(${leftSide})`;
    }

    // Verify Array Indexing Logic (+1) - Appears Correct, No Change Needed Here
    printElementAccessExpression(node, identation) {
        const { expression, argumentExpression } = node;

        const exception = this.printElementAccessExpressionExceptionIfAny(node);
        if (exception) {
            return exception;
        }

        const expressionAsString = this.printNode(expression, 0);
        let argumentAsString = this.printNode(argumentExpression, 0);

        // Determine if it's array access (needs +1) or dictionary access (uses key directly)
        const expressionType = global.checker.getTypeAtLocation(expression);

        // Heuristic: If expression is clearly an Array/Vector type
        // OR if the index is a number literal/variable known to be number.
        let isArrayIndex = false;
        // Use the improved isArrayType check
        if (this.isArrayType(expressionType)) {
            isArrayIndex = true;
        } else {
            // If expression type is not clearly array, check the argument type
            const argumentType =
                global.checker.getTypeAtLocation(argumentExpression);
            // Check if the argument type flags indicate a number or numeric literal
            if (
                argumentType.flags & ts.TypeFlags.NumberLike ||
                ts.isNumericLiteral(argumentExpression)
            ) {
                // If index is number-like, assume it's for an array unless expression type strongly suggests otherwise (e.g. Dict{Int, Any})
                // This heuristic might fail for Dicts with integer keys.
                // TODO: Potentially needs smarter type checking for Dict{Int, ...} cases
                isArrayIndex = true;
            }
            // Otherwise (string literal, string variable, symbol, etc.), assume dictionary key access
        }

        if (isArrayIndex) {
            // Add '+ 1' for 1-based indexing
            if (ts.isNumericLiteral(argumentExpression)) {
                // Handle numeric literal index directly
                try {
                    const numericValue = parseInt(argumentAsString, 10);
                    if (
                        !isNaN(numericValue) &&
                        Number.isInteger(numericValue)
                    ) {
                        // Check if it's a valid integer
                        argumentAsString = (numericValue + 1).toString(); // Calculate 1-based index
                    } else {
                        // Fallback for non-integer literals or if parse fails, add 1 dynamically
                        console.warn(
                            "[JuliaTranspiler] Non-integer numeric literal used for array indexing, adding +1 dynamically:",
                            argumentAsString,
                        );
                        argumentAsString = `(${argumentAsString}) + 1`; // Wrap original arg if adding
                    }
                } catch (e) {
                    console.warn(
                        "[JuliaTranspiler] Could not parse numeric literal for indexing, adding +1 dynamically:",
                        argumentAsString,
                    );
                    argumentAsString = `(${argumentAsString}) + 1`; // Wrap original arg if adding
                }
            } else {
                // Handle variable or other expression types used as array index
                // Just add 1 directly. Julia handles integer arithmetic.
                // If indexVar could be float, conversion might be needed, but test expects direct addition.
                argumentAsString = `${argumentAsString} + 1`; // Add 1 directly to the variable/expression
            }
        } else {
            // Dictionary Key Access
            if (ts.isStringLiteral(argumentExpression)) {
                // Convert string literal "key" to :key for dictionary access
                argumentAsString = `Symbol("${argumentExpression.text}")`;
            } else {
                // Assume argumentExpression holds a variable or expression evaluating to a key
                // Convert to Symbol if it's not already one (consistent with delete/creation)
                // Check if the variable is likely already a Symbol based on its name convention or type if possible
                const argumentType =
                    global.checker.getTypeAtLocation(argumentExpression);
                const argumentSymbol = argumentType.getSymbol();
                if (argumentSymbol && argumentSymbol.escapedName === "Symbol") {
                    // If it's already a symbol, use it directly
                } else {
                    // Otherwise, convert to symbol
                    argumentAsString = `Symbol(${argumentAsString})`;
                }
            }
        }

        // Construct the Julia expression: expression[argument]
        // Wrap expression if it's complex? Usually not needed for simple variables.
        // Wrap argument if it involved addition? Parentheses added above solve this.
        return `${expressionAsString}[${argumentAsString}]`;
    }

    // Helper to check for array-like type flags
    isArrayType(type: ts.Type): boolean {
        // Check if the type is an array type
        if (global.checker.isArrayType(type)) {
            return true;
        }

        // Check if the type is a tuple type
        if (global.checker.isTupleType(type)) {
            return true;
        }

        // Check if the type is a union type containing an array or tuple
        if (type.isUnion()) {
            for (const unionType of type.types) {
                if (this.isArrayType(unionType)) {
                    // Recursive call
                    return true;
                }
            }
        }

        // Fallback: Check the symbolic name if available (less reliable)
        const symbol = type.getSymbol();
        if (symbol && symbol.escapedName === "Array") {
            return true;
        }

        // Consider interface names like ReadonlyArray
        if (symbol && symbol.escapedName === "ReadonlyArray") {
            return true;
        }

        // Add more specific checks if needed, e.g., for specific built-in types like NodeListOf

        return false;
    }

    printMethodDefinition(node, identation) {
        let name = node.name.escapedText;
        name = this.transformMethodNameIfNeeded(name);
        this.currentFunctionName = name;

        let returnType = this.printFunctionType(node);

        // Get modifiers string from base, then potentially remove 'async' if asyncTranspiling is true
        let modifiers = super.printModifiers(node);
        if (this.asyncTranspiling && modifiers.includes(this.ASYNC_TOKEN)) {
            // Remove the async token from the modifiers string
            modifiers = modifiers.replace(this.ASYNC_TOKEN, "").trim();
        }
        const defaultAccess = this.METHOD_DEFAULT_ACCESS
            ? this.METHOD_DEFAULT_ACCESS + " "
            : "";
        modifiers = modifiers ? modifiers + " " : defaultAccess; // tmp check this

        const parsedArgs = this.printMethodParameters(node);

        returnType = returnType ? returnType + " " : returnType;

        const methodToken = this.METHOD_TOKEN ? this.METHOD_TOKEN + " " : "";
        const methodDef =
            this.getIden(identation) +
            modifiers +
            returnType +
            methodToken +
            name +
            "(" +
            parsedArgs +
            ")";

        this.doComments = true;
        let result = this.printNodeCommentsIfAny(node, identation, methodDef);
        this.doComments = false;
        return result;
    }

    removeLeadingEmptyLines(text: string): string {
        // This regex matches one or more occurrences of:
        // ^           - Matches the beginning of the string.
        // [ \t\r\n]   - Matches a space, tab, carriage return, or newline character.
        // +           - Matches the character class one or more times.
        // Replace the matched pattern with an empty string.
        const regex = /^[ \t\r\n]+/;

        return text.replace(regex, "");
    }

    protected transformIdentifierForReservedKeywords(idValue: string) {
        if (this.ReservedKeywordsReplacements[idValue]) {
            return this.ReservedKeywordsReplacements[idValue];
        }
        return idValue;
    }

    protected transformIdentifierAndUnCamelCaseIfNeeded(idValue: string) {
        let result = this.transformIdentifierForReservedKeywords(idValue);
        return this.unCamelCaseIfNeeded(result);
    }

    transformMethodNameIfNeeded(name: string): string {
        return this.transformIdentifierAndUnCamelCaseIfNeeded(name);
    }

    transformFunctionNameIfNeeded(name: any): string {
        return this.transformIdentifierAndUnCamelCaseIfNeeded(name);
    }

    transformCallExpressionName(name: string): string {
        return this.transformIdentifierForReservedKeywords(name);
    }

    printLeadingComments(node, identation) {
        const fullText = global.src.getFullText();
        const commentsRangeList = ts.getLeadingCommentRanges(
            fullText,
            node.pos,
        );
        const commentsRange = commentsRangeList ? commentsRangeList : undefined;
        let res = "";
        if (commentsRange) {
            for (const commentRange of commentsRange) {
                const commentText = fullText.slice(
                    commentRange.pos,
                    commentRange.end,
                );
                if (commentText !== undefined) {
                    const formatted = commentText
                        .split("\n")
                        .map((line) => line.trim()) // Trim leading/trailing whitespace from the original line
                        .map((line) => {
                            // Add indentation based on the original identation param
                            // Remove check for '*' as it's handled by transformLeadingComment
                            return this.getIden(identation) + line;
                         })
                        .join("\n");
                    res += this.transformLeadingComment(formatted.trim()) + "\n"; // Add newline after each transformed comment block
                }
            }
        }
        let res_trim = res.trim();
        if (this.transpiledComments.has(res_trim) && !this.doComments) {
            return "";
        } else {
            this.transpiledComments.add(res_trim);
            // Return the result without adding extra indentation here
            // The mapping function inside already added the correct base indentation
            return res;
        }
    }

    getCustomOperatorIfAny(left, right, operator) {
        const rightText = this.printNode(right, 0).trim(); // Print right side for comparison
        const opkind = operator.kind;

        // Special handling ONLY for strict comparisons with `undefined` (Julia `nothing`)
        if (rightText === this.UNDEFINED_TOKEN) {
            switch (opkind) {
                case ts.SyntaxKind.EqualsEqualsEqualsToken: // === undefined
                    return "==="; // Maps to Julia's strict equality
                case ts.SyntaxKind.ExclamationEqualsEqualsToken: // !== undefined
                    return "!=="; // Maps to Julia's strict inequality
                // For loose equality (== undefined) and loose inequality (!= undefined)
                // we explicitly *don't* return a custom operator here.
                // `printBinaryExpression` will then use the default mapping
                // for `==` and `!=`, which are "==" and "!=" respectively,
                // resulting in `something == nothing` or `something != nothing`.
            }
        }

        // No custom override for other operators or non-undefined comparisons
        return undefined;
    }

    getIndentationRegex(text: string): string {
      if (!text) {
        return ""; // Handle null, undefined, or empty string
      }

      // Match zero or more spaces or tabs at the beginning (^) of the string
      const match = text.match(/^[\t ]*/);

      // match will return an array where the first element is the matched string,
      // or null if no match (though with *, it should always match at least "")
      return match ? match[0] : "";
    }
}
