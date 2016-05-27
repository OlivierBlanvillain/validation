#!/bin/bash
set -eux

sbt ++$TRAVIS_SCALA_VERSION clean test

bash misc/build-book.sh

(
  cd play-scalajs-example
  sbt ++$TRAVIS_SCALA_VERSION compile
)
