
# Determine the directory of the init.jl script
script_dir = @__DIR__

# Construct full paths to the helper and transpilable files
helpers_path = joinpath(script_dir, "helpers.jl")
transpilable_path = joinpath(script_dir, "transpilable.jl")

# Include helper functions first
try
    include(helpers_path)
catch e
    println(stderr, "Error including helpers.jl: $e")
    rethrow()
end

# Include the main transpiled code
try
    include(transpilable_path)
catch e
    println(stderr, "Error including transpilable.jl: $e")
    rethrow()
end


# --- Main Execution ---
function main()
    # Assuming the transpiler created the 'Test' struct and 'test' function as above
    println(stderr, "Instantiating Test...") # Debug message
    instance = Test()

    println(stderr, "Calling test function...") # Debug message
    try
        # Call the test function, passing the instance
        instance.test(instance)
        println(stderr, "Test function finished.") # Debug message
    catch e
        println(stderr, "Error during test execution: $e")
        # Print stack trace for debugging
        showerror(stderr, e, catch_backtrace())
        println(stderr) # Newline after stacktrace
        exit(1) # Exit with error code if test fails
    end
end

# Run the main function
println(stderr, "Starting Julia integration test execution...") # Debug message
main()
println(stderr, "Julia integration test execution finished.") # Debug message
