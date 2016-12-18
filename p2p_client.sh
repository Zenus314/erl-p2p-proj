#!/bin/bash

# This script run a client on the machine 8${1} for user ${2}.
~/mpe/erl/teda/scripts/run.sh p2p "client:start()" hosts_alive.conf diufpc8${1}.unifr.ch ${2}

