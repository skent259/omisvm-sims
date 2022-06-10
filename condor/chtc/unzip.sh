#!/bin/bash

# Unzips the directories returned from condor
name="size-swd-4.0.0-1"
find . -name "sim-$name*.tar.gz" -exec tar -xzf {} \;