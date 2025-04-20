# ast-transpiler/tests/integration/julia/helpers.jl

using Printf # For potential formatting needs, though not strictly used in this version

# --- Basic Type Checks (Can be expanded) ---
_is_number(x) = isa(x, Number)
_is_string(x) = isa(x, AbstractString)
_is_bool(x) = isa(x, Bool)
_is_array(x) = isa(x, AbstractArray)
_is_dict(x) = isa(x, AbstractDict)

# --- Mimic JS Operators / Functions ---

# add: Handles numeric addition and string concatenation
function add(a, b)
    if _is_number(a) && _is_number(b)
        return a + b
    elseif _is_string(a) || _is_string(b) # Be flexible if one is string
        return string(a) * string(b) # String concat is *
    else
        # Fallback or error? Let's try promotion for simplicity, might fail
        try
            return +(promote(a, b)...)
        catch
            @warn "add: Unsupported types or incompatible for addition/concatenation: $(typeof(a)), $(typeof(b))"
            return nothing
        end
    end
end

# isTrue: Mimics JavaScript truthiness
function isTrue(val)
    if val === false || val === nothing || val == 0 || val == ""
        return false
    elseif isa(val, AbstractArray) || isa(val, AbstractDict)
        return !isempty(val)
    # Add other specific falsey cases if needed (e.g., NaN?)
    else
        # All other values (non-empty strings, non-zero numbers, true, non-empty collections, etc.) are truthy
        return true
    end
end

# getArrayLength: Length of Array, String, Dict
function getArrayLength(val)
    if val === nothing
        return 0
    end
    try
        return length(val) # Works for AbstractArray, AbstractString, AbstractDict
    catch
        @warn "getArrayLength: Could not get length for type $(typeof(val))"
        return 0
    end
end

# getValue: Access elements (IMPORTANT: Handles 0-based index from JS -> 1-based for Julia)
function getValue(collection, key)
    if collection === nothing
        return nothing
    end
    try
        if isa(collection, AbstractArray)
            # Convert JS 0-based index to Julia 1-based index
            idx = Int(key) + 1
            if checkbounds(Bool, collection, idx)
                return collection[idx]
            else
                # @warn "getValue: Index $idx out of bounds for array of length $(length(collection))"
                return nothing # JS-like behavior for out-of-bounds
            end
        elseif isa(collection, AbstractDict)
             # Key needs to be the correct type for the Dict, attempt conversion if string key
             typed_key = isa(key, String) ? try convert(keytype(collection), key) catch; key end : key
             return get(collection, typed_key, nothing) # Return nothing if key doesn't exist
        elseif isa(collection, String)
             # Convert JS 0-based index to Julia 1-based index
             idx = Int(key) + 1
             if checkbounds(Bool, collection, idx)
                return string(collection[idx]) # Return Character as String
             else
                 # @warn "getValue: Index $idx out of bounds for string of length $(length(collection))"
                 return nothing
             end
        else
            @warn "getValue: Unsupported collection type $(typeof(collection))"
            return nothing
        end
    catch e
        @warn "getValue: Error accessing collection with key '$key': $e"
        return nothing
    end
end

# --- Comparisons ---
# Use try-catch for robustness if transpiler might allow comparing different types
isLessThan(a, b) = try a < b catch; false end
isGreaterThan(a, b) = try a > b catch; false end
# isEqual: Use == for value comparison. === is for type and value identity.
isEqual(a, b) = try a == b catch; false end

# --- In-place Operations ---

# reverse_inplace!: Reverses Vectors/Arrays
function reverse_inplace!(arr)
    if isa(arr, AbstractVector)
        try
            reverse!(arr)
        catch e
             @warn "reverse_inplace!: Failed to reverse array: $e"
        end
    else
        @warn "reverse_inplace!: Input is not an AbstractVector, type: $(typeof(arr))"
    end
    # No return value needed for in-place
end

# removeKey!: Removes key from Dictionary
function removeKey!(dict, key)
    if isa(dict, AbstractDict)
        try
            # Attempt to convert key type if dict expects something specific
            typed_key = isa(key, String) ? try convert(keytype(dict), key) catch; key end : key
            delete!(dict, typed_key)
        catch e
             @warn "removeKey!: Failed to remove key '$key': $e"
        end
    else
         @warn "removeKey!: Input is not an AbstractDict, type: $(typeof(dict))"
    end
     # No return value needed for in-place
end

# getDictKeys: Get dictionary keys as an Array
function getDictKeys(dict)
    if isa(dict, AbstractDict)
        return collect(keys(dict))
    else
        @warn "getDictKeys: Input is not an AbstractDict, type: $(typeof(dict))"
        return [] # Return empty array
    end
end

function toString(arg)
    if arg === nothing
        return "nothing" # Or "null" if strict JS compatibility needed
    elseif isa(arg, Bool)
        return arg ? "true" : "false"
    elseif isa(arg, AbstractArray)
        # Mimic JS array-to-string more closely (comma-separated elements)
        # Handle nested arrays recursively? For simplicity, maybe just join top-level
        return join(map(toString, arg), ",") # JS uses comma
    elseif isa(arg, AbstractDict)
        # JS default is often '[object Object]'
        return "[object Object]" # Simple mimicry
    else
        return string(arg)
    end
end

# concat: Concatenate arrays/vectors
function concat(a, args...) # Takes the initial 'value' `a` and subsequent arguments
    # Check the type of the first argument 'a' (mimicking 'this' in JS)
    if isa(a, AbstractString)
        # --- String Concatenation ---
        # Start with the initial string 'a'
        result_str = string(a)
        # Concatenate string representations of all subsequent arguments
        for arg in args
            result_str *= toString(arg) # Use our toString for consistent conversion
        end
        return result_str

    else
        # --- Array Concatenation (or treat 'a' as first element) ---
        # Initialize result as Vector{Any} to hold potentially mixed types
        result_arr = Any[]

        # Handle the first argument 'a'
        if a === nothing
            # Option 2 from original: Treat `nothing` as an element
            push!(result_arr, nothing)
        elseif isa(a, AbstractVector)
            # If 'a' is a vector, append its elements
            append!(result_arr, a)
        else
            # If 'a' is not a vector (and not a string handled above),
            # add it as a single element (mimics [a].concat(...))
            push!(result_arr, a)
        end

        # Process the rest of the arguments
        for arg in args
            if arg === nothing
                # Append nothing directly (like JS appends null/undefined)
                push!(result_arr, nothing)
            elseif isa(arg, AbstractVector)
                # If arg is a vector, append its elements
                append!(result_arr, arg)
            else
                # If arg is not a vector, add it as a single element
                push!(result_arr, arg)
            end
        end
        return result_arr
    end
end
# replaceAll: String replacement
function replaceAll(s, old, new)
    if !_is_string(s) || !_is_string(old) || !_is_string(new)
        @warn "replaceAll: Inputs must be strings. Got: $(typeof(s)), $(typeof(old)), $(typeof(new))"
        return s # Return original string if types are wrong
    end
    return replace(s, old => new)
end

# println_wrapper: Simple wrapper for consistent output newline like console.log
# Uses Printf.@printf for more format control if needed later, but println is fine now.
# println_wrapper(x...) = println(x...)
function println_wrapper(args...)
    # Convert common types to string representations for printing
    str_args = map(arg -> begin
        if arg === nothing
            # Decide how to print nothing, "nothing" or "null"? Let's use "nothing" for Julia context
            "nothing"
        elseif isa(arg, Bool)
            arg ? "true" : "false" # JS often uses lowercase bool strings
        else
            string(arg) # Default string conversion
        end
    end, args)
    println(join(str_args, " ")) # Join multiple arguments with spaces like console.log
end


# postFixIncrement: Needs careful handling by the transpiler.
# Julia doesn't have direct equivalent of x++ mutation within an expression or C# ref.
# Transpiler should generate `w = postFixIncrement(w)`
function postFixIncrement(a)
    if _is_number(a)
        # The function *returns* the incremented value.
        # The *original* variable `a` in the caller's scope is *not* changed by this alone.
        return a + 1
    else
        @warn "postFixIncrement: Input is not a number, type: $(typeof(a))"
        return a # Return original value if not a number
    end
end

# toString: Equivalent to JS .toString() or String()
# Ensure consistent string conversion, especially for bools or nothing
function toString(arg)
    if arg === nothing
        return "nothing" # Or "null" if strict JS compatibility needed
    elseif isa(arg, Bool)
        return arg ? "true" : "false"
    else
        return string(arg)
    end
end
