#!/bin/bash

export EGTB=./egtb
export EGTBTEMP=./egtb-test
TEST=./egtb-test
set -e

cargo build && cargo build --release
./target/release/rasch gen K-K
# 3er
for f in Q R B N P 
do
    for SIG in K$f-K K-K$f
    do
        ./target/release/rasch gen $SIG
    done
done

# 4er
for f in Q R B N P
do
    for g in Q R B N P
    do
        for SIG in  K$f$g-K  K$f-K$g  K-K$f$g
        do
            ./target/release/rasch gen $SIG
        done
    done
done

# 5er
for f in Q R B N P
do
    for g in Q R B N P
    do
        for h in Q R B N P
        do
            for SIG in  K$f$g$h-K K$f$g-K$h K$f-K$g$h K-K$f$g$h
            do
                ./target/release/rasch gen $SIG
            done
        done
    done
done

#6er
for f in Q R B N P
do
    for g in Q R B N P
    do
        for h in Q R B N P
        do
            for i in Q R B N P
            do
                for SIG in K$f$g$h$i-K K$f$g$h-K$i K$f$g-K$h$i  K$f-K$g$h$i K-K$f$g$h$i
                do
                    ./target/release/rasch gen $SIG 
                done
            done
        done
    done
done
