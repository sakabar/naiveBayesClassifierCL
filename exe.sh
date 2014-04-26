#!/bin/zsh

# mypath=./target/scala-2.10/classes/
# cd  mypath
# scala Main



JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF8' sbt run < ./data/test_kakaku.txt
