# TODO - improve and standardise between integration tests

rm -f test_summary.log
rm -f FunctionalCorrectness.vo
rm -f FunctionalCorrectness.glob

# Compile to Coq Test
echo "🛈  Compiling to Coq: dsc ... coq" 1>> test_summary.log
./compile_for_coq.sh; compilationExitCode=$?
if [ $compilationExitCode -eq 0 ]; then
    echo " Compilation Succeeded - This is unexpected - ❌ Test FAILED" 1>> test_summary.log
else
    echo " Compilation Failed - This is expected - ✅ Test Passed" 1>> test_summary.log
    echo "-----SUMMARY------"
    cat test_summary.log
    exit 1
fi

echo "-----SUMMARY------"
cat test_summary.log