#!/bin/bash

ulimit -t 60       # at most one minute of work
ulimit -f 26214400 # 25 MB cap on sizes of generated files
ulimit -n 10       # at most 10 open files
ulimit -d 102400   # 100 MB cap on data segment/malloc'd memory
ulimit -l 0        # no locking memory allowed (weird case)

# can't do these, but I'd like to (I think):
#
# ulimit -p 0        # no piping allowed
# ulimit -s 102400   # 100 MB cap on stack size

if [[ -z "$1" ]]; then
    echo "error: you must specify a TeX friend as the first argument"
    exit 1;
fi

friends="tex pdftex latex pdflatex bibtex bibtex8 amstex"
friend_bin_root="/usr/local/texlive/2009/bin/universal-darwin"

# Ensure that the first argument belongs to the list of friends
friend=""
for x in $friends; do
    if [[ "$1" = "$x" ]]; then
	friend=$x;
	break;
    fi
done

if [[ -z $friend ]]; then
    echo "error: unknown TeX friend: $1"
    exit 1;
fi

# Work directory
if [[ -z "$2" ]]; then
    echo "error: unspecified work directory"
    exit 1;
fi

workdir="$2"

if [[ ! -e $workdir ]]; then
    echo "error: the work directory does not exist: $workdir"
    exit 1;
fi

if [[ ! -d $workdir ]]; then
    echo "error: work directory is not a directory: $workdir"
    exit 1;
fi

if [[ ! -x $workdir ]]; then
    echo "error: directory is not writable: $workdir"
    exit 1;
fi

# Input file
if [[ -z "$3" ]]; then
    echo "error: unspecified file"
    exit 1;
fi

file="$3"
file_full_path="$workdir/$file"

if [[ ! -e $file_full_path ]]; then
    echo "error: file $file does not exist in work directory $workdir"
    exit 1;
fi

if [[ ! -f $file_full_path ]]; then
    echo "error: file $file is not a regular file"
    exit 1;
fi

if [[ -h $file_full_path ]]; then
    echo "error: file $file is a hard link"
    exit 1;
fi

if [[ -L $file_full_path ]]; then
    echo "error: file $file is a symbolic link"
    exit 1;
fi

if [[ ! -r $file_full_path ]]; then
    echo "error: file $file is not readable"
    exit 1;
fi

if [[ -x $file_full_path ]]; then
    echo "error: file $file is executable"
    exit 1;
fi

if [[ -u $file_full_path ]]; then
    echo "error: setuid bit set on $file"
    exit 1;
fi

friend_path="$friend_bin_root/$friend"

if [[ ! -e $friend_path ]]; then
    echo "error: file does not exist: $friend_path"
    exit 1
fi

if [[ ! -x $friend_path ]]; then
    echo "error: program not executable: $friend_path"
    exit 1;
fi

nice $friend_path              \
  -halt-on-error               \
  -no-shell-escape             \
  -output-directory="$workdir" \
  "$file";

exit $?
