# erl-p2p-proj

1. First step execute the script setupEnvironment.sh diufUsername

This script will prepare the environnement sending to all the machines files in order to try the p2p

This is the result of the script:
+---------+------------------------------------------------+
|         |                   Files Name                   |
+ PC Name +------------------------------------------------+
|         | lorem.txt | sam.mp4 | games.mp4 | isengard.mp4 |
+---------+-----------+---------+-----------+--------------+
|   pc80  |    Yes    |   Yes   |    Yes    |      Yes     |
+---------+-----------+---------+-----------+--------------+
|   pc81  |    Yes    |   Yes   |    Yes    |      Yes     |
+---------+-----------+---------+-----------+--------------+
|   pc82  |    Yes    |   Yes   |    Yes    |      Yes     |
+---------+-----------+---------+-----------+--------------+
|   pc83  |    Yes    |   Yes   |    Yes    |      Yes     |
+---------+-----------+---------+-----------+--------------+
|   pc84  |    Yes    |   Yes   |     No    |      No      |
+---------+-----------+---------+-----------+--------------+
|   pc85  |    Yes    |   Yes   |     No    |      No      |
+---------+-----------+---------+-----------+--------------+
|   pc86  |     No    |    No   |     No    |      No      |
+---------+-----------+---------+-----------+--------------+

2. Launch ./server.sh diufUsername

This script will send to all pc the code and launch a server in the pc80

3. In a new terminal launch ./client.sh diufUsername NumberOfThePC

Every pc can be a client also the pc with the server.
Just open for every client a new terminal.
Then try to download something

4. Execute deleteShared.sh diufUsername to restore to the starting state the enironnement
