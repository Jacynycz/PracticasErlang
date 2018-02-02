#! /bin/bash 
if [ -z "$1" ]
  then
      echo "Usage './node.sh <nodename>'"
    exit 0
fi
IP=$(ip route get 8.8.8.8 | awk '/src / { print $7 } ')
erl -name $1@$IP

