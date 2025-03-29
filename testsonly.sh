#!/bin/bash

# Check if a filename is provided as an argument
if [ -z "$1" ]; then
    echo "Usage: $0 <filename> [num_tests_to_only]"
    exit 1
fi

filename="$1"
num_tests_to_only="${2:-1}"  # Default to 1 if not provided


# Function to add or remove .only
modify_test() {
    local line="$1"
    local action="$2"

    if [[ "$line" == *"test.only"* ]]; then
        if [[ "$action" == "remove" ]]; then
            echo "${line/test.only/test}"
        else
            echo "$line"
        fi
    elif [[ "$line" == *"test("* ]]; then
        if [[ "$action" == "add" ]]; then
            echo "${line/test(/test.only(}"
        else
            echo "$line"
        fi
    else
        echo "$line"
    fi
}


if [[ "$num_tests_to_only" == "0" ]]; then
    # Remove .only from all tests
    sed -i "s/test\.only/test/g" "$filename"
else
    # Add .only to a specific number of tests
    count=0
    tmpfile=$(mktemp)

    while IFS= read -r line; do
        if [[ "$line" == *"test("* ]]; then
            if [[ "$count" -lt "$num_tests_to_only" ]]; then
                modified_line=$(modify_test "$line" "add")
            else
                modified_line=$(modify_test "$line" "remove")
            fi
            count=$((count + 1))
        else
            modified_line="$line"
        fi
        echo "$modified_line" >> "$tmpfile"
    done < "$filename"

    mv "$tmpfile" "$filename"
fi

echo "File '$filename' modified successfully."
