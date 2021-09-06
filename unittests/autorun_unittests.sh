#!/bin/bash

conflux=../../conflux-rust

if [ ! -d $conflux ]
then
pushd ../..
git clone https://github.com/Conflux-Chain/conflux-rust.git
cd conflux-rust
git checkout v1.1.5
cargo build --release
popd
fi

setsid sh -c "npm run ganache > /dev/null & ./start_conflux.sh $conflux > /dev/null" &
pgid=$!
sleep 10
./run_unittests.sh
kill -TERM -$pgid