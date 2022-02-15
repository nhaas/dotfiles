#!/bin/bash

p4depot()
{
    cwd=$PWD
    depot=`p4 client -o $USER.$1 | grep "$USER.$1/\.\.\." | sed 's/.*\(\/\/.*\.\.\.\).*\.\.\./\1/'`
    builtin cd $cwd
}

p4integrate()
{
    if [ $# -lt 2 ]; then
        echo "p4integrate <source> <dest>"
        echo "  This does a integrate of all changelists submitted on <source> to <dest>."
        echo "  The source/dest name is as they appear in the directory."
        return 1
    fi

    for i in $1 $2; do
        if [ ! -d "$i" ]; then
            echo "Couldn't find directory $i"
            return 1
        fi
    done

    p4depot $1
    sdepot=$depot
    p4depot $2
    ddepot=$depot

    echo "src depot = '$sdepot'"
    echo "dst depot = '$ddepot'"

    cls=(`p4 changes $sdepot |sort|sed '1d'|cut -d' ' -f2`)

    if [ "${#cls[@]}" -eq 0 ]; then
        echo "No changelists available on $sdepot"
        return 1;
    fi

    first="${cls[0]}"
    last="${cls[${#cls[@]}-1]}"

    echo "Integrating"
    echo "   $sdepot@$first-$last"
    echo "-> $ddepot"
    echo "This might take some time, be patient..."

    echo p4 -c $USER.$2 integrate -q $sdepot@$first,$last $ddepot
    # p4 -c $USER.$2 integrate -q $sdepot@$first,$last $ddepot
}
