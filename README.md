# erl-p2p-proj

Usiamo questa repository solo per condividere i tre (per ora) file di codice.

Per usarli, occorre salvarli nell'ambiente teda, sotto la cartella seguente:
~/mpe/erl/teda/apps/p2p/

28.11.2016:
- I client possono connettersi al server.

Prossimo passo:
- I client devono saper riconoscere quali file hanno a disposizione nella cartella apposita e inviare l'informazione al server.
[La cartella potrebbe essere .../p2p/shared_files e il client deve ottenere i nomi dei file come stringhe.]
- Il server deve avere a disposizione una struttura dati che permette di sapere quali file sono disponibili nella rete e da quali macchine (o meglio, processi).
[Immagino una cosa tipo una lista di tuple (nome_file,[lista dei pid che condividono il file]).]
