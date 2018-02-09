if [ -z "$1" ]
then
    echo "Usage './topology.sh <terminal_command>'"
    exit 0
fi
$1 -e "erl -sname 1" 
$1 -e "erl -sname 2"
$1 -e "erl -sname 3"
$1 -e "erl -sname 4"
$1 -e "erl -sname 5"
$1 -e "erl -sname 6"
