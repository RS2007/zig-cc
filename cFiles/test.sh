# Run every c s file pair
# check if the compiled executed out of them are equal

# loop through the dir
# have a c folder and an asm folder 
# iterate through the c folder, get basename
# find the .s file in the s folder,
# compile both via gcc
# execute
# check if there is a difference in output


C_DIR="./C"
S_DIR="./S"

for file in "$C_DIR"/*.c; do
    echo "$file"
    base_name=${file#$C_DIR/}
    base_name=${base_name%.c}
    echo "$base_name"
    
    gcc -o "c$base_name" "$file"
    ./"c$base_name"
    cExitCode=$?
    
    gcc -o "s$base_name" "$S_DIR/$base_name.s"
    ./"s$base_name"
    sExitCode=$?
    if [ $cExitCode -ne $sExitCode ]; then
      exit 1
    fi
done
echo "All tests passed"
