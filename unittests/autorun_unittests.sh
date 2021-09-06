#!/bin/bash

npm install

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
kill_subprocesses() {
    echo "Also killing subprocesses: ganache and conflux"
    kill -TERM -$pgid
    trap - SIGINT SIGTERM
    kill -- -$$ 
}
trap kill_subprocesses SIGINT SIGTERM
sleep 10
./run_unittests.sh
kill -TERM -$pgid

if [ ! -f "expected_output.log" ]; then
    echo "ERROR: expected_output.log does not exist. Generate via running this script then renaming test_output.log to expected_output.log, if the tests were successful."
    exit 1
fi
if [ ! -f "test_output.log" ]; then
    echo "ERROR: test_output.log file not generated."
    exit 1
fi
if ! cmp -s test_output.log expected_output.log
then
  echo "FAILED ❌: Unit tests output different to expected. See expected_output.log and test_output.log"
  echo "If necessary, reenerate via running this script then renaming test_output.log to expected_output.log, if the tests were successful."
  diff test_output.log expected_output.log
  exit 1
else
  echo "All unit tests passed ✅"
fi