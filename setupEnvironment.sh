#!/bin/bash

#if [-z "$1"]
  #then echo "Put as parameter your diuf username"
  #else 
      for i in `seq 0 7`;
          do  
              ssh ${1}@diufpc8${i}.unifr.ch "mkdir -p mpe/erl/teda/p2p_shared_files"

          done 

       #list of file to send
       scp ../../p2p_shared_files/sam ${1}@diufpc84.unifr.ch:mpe/erl/teda/p2p_shared_files/
#fi
