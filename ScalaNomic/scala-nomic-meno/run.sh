#!/bin/bash

mkdir TEST
cp src/main/resources/meno.meno TEST/com-test-meno.meno
mvn scala:run -DaddArgs=TEST/com-test-meno.meno
cd TEST/com-test-meno
mvn test
mvn scala:run -DaddArgs=../com-test-meno.meno

