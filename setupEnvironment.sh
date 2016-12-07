#!/bin/bash

#if [-z "$1"]
  #then echo "Put as parameter your diuf username"
  #else 
      for i in `seq 0 7`;
          do  
              ssh ${1}@diufpc8${i}.unifr.ch "mkdir -p mpe/erl/teda/p2p_shared_files"
          done 

       #list of file to send
       for i in `seq 0 6`;
          do  
              scp ../../p2p_shared_files/southpath.avi ${1}@diufpc84.unifr.ch:mpe/erl/teda/p2p_shared_files/
              scp ../../p2p_shared_files/sam.mp4 ${1}@diufpc84.unifr.ch:mpe/erl/teda/p2p_shared_files/
          done
      
       for i in `seq 0 3`;
          do 
              scp ../../p2p_shared_files/isengard.mp4 ${1}@diufpc84.unifr.ch:mpe/erl/teda/p2p_shared_files/
              scp ../../p2p_shared_files/games.mp4 ${1}@diufpc84.unifr.ch:mpe/erl/teda/p2p_shared_files/
          done
#fi
