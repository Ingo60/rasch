#!/bin/bash

export EGTB=./egtb-test
# export EGTBTEMP=./egtbtemp
set -e

cargo build && cargo build --release
./target/release/rasch make K-K
# 3er
for f in Q R B N P 
do
    ./target/release/rasch make K$f-K
    ./target/release/rasch make K-K$f
done

# 4er
for f in Q R B N P
do
    for g in Q R B N P
    do
        ./target/release/rasch make K$f$g-K
        ./target/release/rasch make K$f-K$g
        ./target/release/rasch make K-K$f$g
    done
done

# 5er
for f in Q R B N P
do
    for g in Q R B N P
    do
        for h in Q R B N P
        do
            ./target/release/rasch make K$f$g$h-K
            ./target/release/rasch make K$f$g-K$h
            ./target/release/rasch make K$f-K$g$h
            ./target/release/rasch make K-K$f$g$h
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
                ./target/release/rasch make K$f$g$h$i-K
                ./target/release/rasch make K$f$g$h-K$i
                ./target/release/rasch make K$f$g-K$h$i
                ./target/release/rasch make K$f-K$g$h$i
                ./target/release/rasch make K-K$f$g$h$i
            done
        done
    done
done
