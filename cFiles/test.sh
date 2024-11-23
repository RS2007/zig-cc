#!/bin/bash

C_DIR="./C"
S_DIR="./S"

for file in "$C_DIR"/*.c; do
    echo "$file"
    base_name=${file#"$C_DIR/"}
    base_name=${base_name%.c}
    echo "$base_name"
    
    gcc -o "c$base_name" "$file" -lm
    ./"c$base_name"
    cExitCode=$?
    
    gcc -o "s$base_name" "$S_DIR/$base_name.s" -lm
    ./"s$base_name"
    sExitCode=$?
    if [ $cExitCode -ne $sExitCode ]; then
      exit 1
    fi
done
echo "All tests passed"
