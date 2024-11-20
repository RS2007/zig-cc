docker rm -vf $(docker ps -a -q)
docker build -t zig-cc-testing .
#docker run -it zig-cc-testing /bin/bash
docker run -it zig-cc-testing /bin/bash test.sh
