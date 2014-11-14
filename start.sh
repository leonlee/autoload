#!/bin/bash 
erl -pa "./ebin" -pz "./dev_patch"  -sname autoload -s autoload -detached