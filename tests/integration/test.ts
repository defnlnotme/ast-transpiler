import { Transpiler } from '../../src/transpiler.js';
import * as fs from 'fs';
import { exec } from "node:child_process";
import { green, yellow, red, blue } from "colorette";
const { readFileSync, writeFileSync } = fs;

// --- File Paths ---
// Source file to be transpiled
const TS_TRANSPILABLE_FILE = "./tests/integration/source/transpilable.ts";
// Target transpiled files
const PY_TRANSPILABLE_FILE = "./tests/integration/py/transpilable.py";
const PHP_TRANSPILABLE_FILE = "./tests/integration/php/transpilable.php";
const CS_TRANSPILABLE_FILE = "./tests/integration/cs/transpilable.cs";
const GO_TRANSPILABLE_FILE = "./tests/integration/go/transpilable.go";
const JL_TRANSPILABLE_FILE = "./tests/integration/julia/transpilable.jl";

// Entry point files for running tests
const TS_FILE = "./tests/integration/source/init.ts";
const PY_FILE = "./tests/integration/py/init.py";
const PHP_FILE = "./tests/integration/php/init.php";
const CS_DIR = "./tests/integration/cs"; // Directory for C# project
const GO_DIR = "./tests/integration/go"; // Directory for Go project
const JL_FILE = "./tests/integration/julia/init.jl";

// --- Language Configuration for Transpiler ---
// Order matters for result indexing!
const langConfig = [
    { language: "csharp", async: true }, // index 0
    { language: "python", async: true }, // index 1
    { language: "php", async: true },    // index 2
    { language: "go", async: true },     // index 3
    { language: "julia", async: true },   // index 4
];

// --- Transpilation Function ---
function transpileTests() {
    console.log(yellow("Starting transpilation..."));
    const parseConfig = {
        "verbose": false,
        "csharp": {
            "parser": {
                "ELEMENT_ACCESS_WRAPPER_OPEN": "getValue(",
                "ELEMENT_ACCESS_WRAPPER_CLOSE": ")",
            }
        },
        "python": { // Added for completeness, adjust if needed
            "parser": {}
        },
        "php": { // Added for completeness, adjust if needed
            "parser": {}
        },
        "go": { // Added for completeness, adjust if needed
            "parser": {}
        },
        "julia": {
             "parser": {
                 "CONSOLE_LOG": "println_wrapper" // Map console.log to helper
                 // Add other Julia-specific parser overrides if needed
             }
        }
    };
    const transpiler = new Transpiler(parseConfig);
    const results = transpiler.transpileDifferentLanguagesByPath(langConfig as any, TS_TRANSPILABLE_FILE);

    // Ensure results array has expected length
    if (results.length !== langConfig.length) {
        throw new Error(`Expected ${langConfig.length} transpilation results, but got ${results.length}`);
    }

    // --- C# Processing ---
    console.log("Processing C# output...");
    let csharp = 'namespace tests;\n' + results[0].content;
    csharp = csharp.replace('class Test', 'partial class Test'); // Assuming Test class exists
    writeFileSync(CS_TRANSPILABLE_FILE, csharp);

    // --- Python Processing ---
    console.log("Processing Python output...");
    const pythonAsync = results[1].content;
    writeFileSync(PY_TRANSPILABLE_FILE, pythonAsync);

    // --- PHP Processing ---
    console.log("Processing PHP output...");
    let phpRes = `<?php\n// Include helpers if needed, or assume autoloader/direct functions\n// require_once 'helpers.php'; // Example if you create PHP helpers\nfunction custom_echo($x){ echo (is_bool($x) ? ($x ? 'true' : 'false') : (string)$x) . "\\n";}\n${results[2].content}\n?>`;
    phpRes = phpRes.replaceAll('var_dump', 'custom_echo'); // Use custom echo for consistent bool/string output
    writeFileSync(PHP_TRANSPILABLE_FILE, phpRes);

    // --- Go Processing ---
    console.log("Processing Go output...");
    const goImports = [
        '\n',
        'import (',
        '    "fmt"',
        ')',
        '\n'
    ].join('\n');
    // Assuming console.log maps to fmt.Println or similar, replace if needed
    let goContent = results[3].content.replace(/fmt\.Println\(/g, 'custom_echo(');
    const go = 'package main\n' + goImports + goContent;
    writeFileSync(GO_TRANSPILABLE_FILE, go);

    // --- Julia Processing ---
    console.log("Processing Julia output...");
    // Assuming result is at index 4 (last in langConfig)
    const juliaContent = results[4].content;
    // No extra wrapping needed here if init.jl handles includes/modules
    writeFileSync(JL_TRANSPILABLE_FILE, juliaContent);

    console.log(green("Transpilation finished."));
}

// --- Command Execution Helper ---
function runCommand(command: string): Promise<string> {
    return new Promise((resolve, reject) => {
        console.log(blue(`Executing: ${command}`)); // Log the command being run
        exec(command, { maxBuffer: 1024 * 1024 * 5 }, (error, stdout, stderr) => {
            // Clean up common non-error stderr messages
            let cleanStderr = stderr || '';
            cleanStderr = cleanStderr.replace(/Debugger attached\.[\s\S]*?Waiting for the debugger to disconnect\.\.\.\n?/g, ''); // Multi-line debugger message
            cleanStderr = cleanStderr.replace(/Debugger listening on ws:\/\/[\s\S]*?For help, see: https:\/\/nodejs\.org\/en\/docs\/inspector\n?/g, ''); // Multi-line inspector message
            // Add specific Julia warning filters if needed, e.g.:
            // cleanStderr = cleanStderr.replace(/Some specific Julia informational message\n/g, '');

            const trimmedStderr = cleanStderr.trim();
            const trimmedStdout = (stdout || '').trim(); // Ensure stdout is trimmed

             if (error) {
                 console.error(red(`Command failed: ${command}`));
                 console.error(red(`Error: ${error.message}`));
                 if (trimmedStderr) console.error(yellow(`Stderr:\n---\n${trimmedStderr}\n---`));
                 if (trimmedStdout) console.log(yellow(`Stdout (error state):\n---\n${trimmedStdout}\n---`));
                 // Reject with a more informative error object
                 reject(new Error(`Command "${command}" failed with code ${error.code}: ${error.message}\nStderr: ${trimmedStderr}`));
                 return;
             }

            // Check stderr even if error is null (exit code 0)
            if (trimmedStderr.length > 0) {
                 // Decide if stderr contents constitute a failure for this specific command/language
                 // For now, warn but resolve successfully if exit code was 0
                 console.warn(yellow(`Command succeeded but produced stderr for "${command}":\n---\n${trimmedStderr}\n---`));
                 // Option: reject(new Error(`Command "${command}" succeeded but produced stderr: ${trimmedStderr}`));
            }
            // Resolve with the trimmed stdout
            resolve(trimmedStdout);
        });
    });
}

// --- Language Specific Runners ---
async function runTS(): Promise<string> {
    const command = "node --no-warnings --loader ts-node/esm " + TS_FILE;
    const result = await runCommand(command);
    console.log(green("Executed TS successfully."));
    return result;
}

async function runPHP(): Promise<string> {
    const command = "php " + PHP_FILE;
    const result = await runCommand(command);
    console.log(green("Executed PHP successfully."));
    return result;
}

async function runPy(): Promise<string> {
    // Ensure python3 is available, or use 'python' if that's the alias
    const command = "python3 " + PY_FILE;
    const result = await runCommand(command);
    console.log(green("Executed PY successfully."));
    return result;
}

async function runCS(): Promise<string> {
    // Use the directory path for dotnet commands
    const buildCommand = `dotnet build --nologo --verbosity quiet "${CS_DIR}"`;
    try {
        await runCommand(buildCommand);
        const runCommandStr = `dotnet run --project "${CS_DIR}/cs.csproj" --no-build`;
        const result = await runCommand(runCommandStr);
        console.log(green("Executed CS successfully."));
        return result;
    } catch (e: any) {
        console.error(red("CS execution failed:"), e.message || e);
        throw e; // Re-throw error to fail the test suite
    }
}

async function runGO(): Promise<string> {
    // 'go run .' within the directory is often simplest
    // Alternatively, list all go files: go run path/to/go/*.go
    const command = `go run ${GO_DIR}/*.go`;
     try {
        const result = await runCommand(command);
        console.log(green("Executed GO successfully."));
        return result;
     } catch (e: any) {
        console.error(red("GO execution failed:"), e.message || e);
        throw e;
     }
}

async function runJL(): Promise<string> {
    // Point Julia to the entry script
    const command = `julia ${JL_FILE}`;
    try {
        const result = await runCommand(command);
        console.log(green("Executed Julia successfully."));
        return result;
    } catch (e: any) {
        console.error(red("Julia execution failed:"), e.message || e);
        throw e;
    }
}

// --- Main Test Execution ---
async function main() {
    console.log("Transpiling tests...");
    transpileTests(); // Generate all language files first

    console.log("\nRunning tests...");

    // Define which tests to run
    const testsToRun: { [key: string]: () => Promise<string> } = {
         'ts': runTS,
         'php': runPHP,
         'py': runPy,
         'cs': runCS,
         'go': runGO,
         'jl': runJL,
    };

    // Select the tests you want to execute and compare
    // const activeTests = ['ts', 'php', 'py', 'cs', 'go', 'jl']; // Run all
    const activeTests = ['ts', 'jl']; // Run only TS and Julia for focused debugging

    const promises = activeTests.map(key => testsToRun[key]());
    const resultsArray = await Promise.all(promises);

    // Map results back to language keys for easier access
    const results: { [key: string]: string } = {};
    activeTests.forEach((key, index) => {
        results[key] = resultsArray[index];
    });

    // Ensure TS baseline exists if needed for comparison
    if (activeTests.includes('ts') && activeTests.length > 1 && !results['ts']) {
         console.error(red("TS baseline execution failed or was not run, cannot compare other languages."));
         process.exit(1);
    }
    const tsOutput = results['ts'] || ''; // Get TS output if available

    let success = true;
    console.log("\n--- Comparing Outputs ---");

    // Loop through active tests (excluding TS itself) for comparison
    for (const lang of activeTests) {
        if (lang === 'ts') continue; // Don't compare TS to itself

        const langOutput = results[lang];
        const langName = lang.toUpperCase(); // e.g., 'JULIA'

        if (langOutput !== tsOutput) {
            success = false;
            compareOutputs(langName, tsOutput, langOutput);
        } else {
            console.log(green(`${langName} output matches TS.`));
        }
    }

    /* // Original comparison structure (kept for reference)
    if (activeTests.includes('php') && results['php'] !== tsOutput) { success = false; compareOutputs("PHP", tsOutput, results['php']); } else if (activeTests.includes('php')) { console.log(green("PHP output matches TS.")); }
    if (activeTests.includes('py') && results['py'] !== tsOutput) { success = false; compareOutputs("PY", tsOutput, results['py']); } else if (activeTests.includes('py')) { console.log(green("Python output matches TS.")); }
    if (activeTests.includes('cs') && results['cs'] !== tsOutput) { success = false; compareOutputs("CS", tsOutput, results['cs']); } else if (activeTests.includes('cs')) { console.log(green("C# output matches TS.")); }
    if (activeTests.includes('go') && results['go'] !== tsOutput) { success = false; compareOutputs("GO", tsOutput, results['go']); } else if (activeTests.includes('go')) { console.log(green("Go output matches TS.")); }
    if (activeTests.includes('jl') && results['jl'] !== tsOutput) { success = false; compareOutputs("Julia", tsOutput, results['jl']); } else if (activeTests.includes('jl')) { console.log(green("Julia output matches TS.")); }
    */

    console.log("\n--- Test Summary ---");
    if (success) {
        console.log(green("✅ Integration tests passed successfully!"));
        process.exit(0); // Explicit success exit code
    } else {
        console.log(red("❌ Integration tests failed."));
        process.exit(1); // Explicit failure exit code
    }
}

// --- Output Comparison Helper ---
function compareOutputs(language: string, tsOutput: string, output: string) {
    console.log(red(`--- ${language} vs TS Output Mismatch ---`));
    console.log(yellow("Expected TS Output:"));
    console.log(`\`\`\`\n${tsOutput}\n\`\`\``);
    console.log(yellow(`Actual ${language} Output:`));
    // Add type info for debugging potential undefined issues
    console.log(`(Type: ${typeof output})`);
    console.log(`\`\`\`\n${output}\n\`\`\``);
    // Add more detailed diff if needed here (e.g., using a diff library)
    console.log(red(`--- End ${language} Mismatch ---`));
}

// --- Script Entry Point ---
main().catch(error => {
    console.error(red("\nUnhandled error during test execution:"), error);
    process.exit(1);
});
