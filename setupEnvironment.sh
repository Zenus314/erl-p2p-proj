#!/bin/bash

# Usage: First arg: diufUsername

for i in `seq 0 6`;
  do  
      ssh ${1}@diufpc8${i}.unifr.ch "mkdir -p mpe/erl/teda/p2p_shared_files"
  done 

#list of file to send
for i in `seq 0 5`;
  do  
      scp ../p2p_shared_files/sam.mp4 ${1}@diufpc8${i}.unifr.ch:mpe/erl/teda/p2p_shared_files/
      scp ../p2p_shared_files/lorem.txt ${1}@diufpc8${i}.unifr.ch:mpe/erl/teda/p2p_shared_files/
  done

for i in `seq 0 3`;
  do 
      scp ../p2p_shared_files/games.mp4 ${1}@diufpc8${i}.unifr.ch:mpe/erl/teda/p2p_shared_files/
      scp ../p2p_shared_files/isengard.mp4 ${1}@diufpc8${i}.unifr.ch:mpe/erl/teda/p2p_shared_files/
  done
