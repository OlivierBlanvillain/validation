#!/bin/bash
set -eux

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

test_cmd="$sbt_cmd clean test"

coverage="$sbt_cmd clean coverage validationJVM/test && sbt coverageReport && sbt coverageAggregate && bash <(curl -s https://codecov.io/bash)"

compile_example="(cd play-scalajs-example; $sbt_cmd compile)"

compile_doc="bash misc/build-book.sh"

run_cmd="$coverage && $test_cmd && $compile_example && $compile_doc"

eval $run_cmd
