podman rm -vf $(podman ps -a -q)
podman build -t zig-cc-testing .
#podman run -it zig-cc-testing /bin/bash
podman run -it zig-cc-testing /bin/bash test.sh
