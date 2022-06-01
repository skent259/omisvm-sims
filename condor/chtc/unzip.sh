#!/bin/bash

# Unzips the directories returned from condor
name="per-tma-2.0.0-1"
find . -name "sim-$name*.tar.gz" -exec tar -xzf {} \;