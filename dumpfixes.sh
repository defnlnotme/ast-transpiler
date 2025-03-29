#!/bin/bash

# Script to dump commit messages and file changes from the last N commits
# to a text file, excluding changes in the tests folder.

# Check if the number of commits (N) is provided as an argument
if [ -z "$1" ]; then
  echo "Usage: $0 <number_of_commits> [output_file]"
  echo "  <number_of_commits>: The number of last commits to analyze."
  echo "  [output_file]: (Optional) The name of the output text file. Defaults to git_changes_log.txt"
  exit 1
fi

num_commits="$1"

# Check if the first argument is a positive integer
if ! [[ "$num_commits" =~ ^[0-9]+$ ]] || [ "$num_commits" -le 0 ]; then
  echo "Error: Number of commits must be a positive integer."
  exit 1
fi

# Set output file name, default to git_changes_log.txt if not provided as second argument
output_file="git_changes_log.txt"
if [ -n "$2" ]; then
  output_file="$2"
fi

echo "Dumping changes from the last $num_commits commits to '$output_file' (excluding files in tests folder)..."

# Check if we are in a git repository
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Error: Not in a git repository."
  exit 1
fi

# Get the last N commit hashes
commit_hashes=$(git log -n "$num_commits" --format="%H")

# Redirect output to the specified file
> "$output_file"  # Create or clear the output file

# Loop through each commit hash
while read -r commit_hash; do
  echo "Commit: $commit_hash" >> "$output_file"

  # Get commit message
  commit_message=$(git log -n 1 --format="%B" "$commit_hash")
  echo "Message:" >> "$output_file"
  echo "$commit_message" >> "$output_file"
  echo "" >> "$output_file" # Add a blank line after the message

  # Get list of changed files in this commit, excluding those in the tests folder
  changed_files=$(git show --name-only --pretty=format: "$commit_hash" | grep -v "^tests/")

  # If there are changed files outside of tests folder
  if [ -n "$changed_files" ]; then
    # For each non-test file that was changed
    while read -r file; do
      if [ -n "$file" ]; then
        # Get the diff for just this file
        echo "File: $file" >> "$output_file"
        git show -p --no-color "$commit_hash" -- "$file" >> "$output_file"
        echo "" >> "$output_file"
      fi
    done <<< "$changed_files"
  else
    echo "No changes outside of tests folder in this commit." >> "$output_file"
  fi

  echo "----------------------------------------" >> "$output_file" # Separator between commits

done <<< "$commit_hashes"

echo "Changes dumped to '$output_file' (tests folder excluded)"

exit 0
