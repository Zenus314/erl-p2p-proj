#!/bin/bash

# Usage: First arg: diuf username

for i in `seq 4 5`;
   do  
      ssh ${1}@diufpc8${i}.unifr.ch "rm -f mpe/erl/teda/p2p_shared_files/isengard.mp4"
      ssh ${1}@diufpc8${i}.unifr.ch "rm -f mpe/erl/teda/p2p_shared_files/games.mp4"
   done


ssh ${1}@diufpc86.unifr.ch "rm mpe/erl/teda/p2p_shared_files/*"
