#!/bin/bash

# Unzips the directories returned from condor
name="size-wq-5.0.0-1d"
find . -name "sim-$name*.tar.gz" -exec tar -xzf {} \;