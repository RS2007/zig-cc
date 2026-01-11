rm -rf ./C/* ./S/*

if command -v gfind >/dev/null 2>&1; then
    FIND=gfind
else
    FIND=find
fi

$FIND . -type f -executable -delete
