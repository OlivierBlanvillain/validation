#!/bin/bash
set -eux

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

test_cmd="$sbt_cmd ci"

coverage="$sbt_cmd coverage ciJVM && sbt coverageReport && sbt coverageAggregate && sbt coveralls"

run_cmd="$coverage && $test_cmd"

eval $run_cmd
