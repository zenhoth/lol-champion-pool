####LoL Champion Pool
This tool uses the champion.gg API to calculate assorted information surrounding your champion pool:
* The best all-round pick that has the best overall winrate against enemy picks, weighted by pick rate (bestAllRounder)
* The best counterpick from your champion pool for each possible enemy champion (bestCounterpicks)
* The best new addition to your champion pool (which new pick will improve your "coverage" of the possible enemy picks, weighted by pick rate) (bestImprovement)

It's not exactly user-friendly. It needs a champion.gg API key (register one on api.champion.gg) in the env variable CHAMPION_GG_API_KEY. Build it with stack. The compiled binary runs bestImprovement and renders the magnitude of each possible improvement:
```
$ stack build --copy-bins
$ env CHAMPION_GG_API_KEY=<your key> lol-champion-pool-exe Middle Annie Syndra TwistedFate
Xerath       ******************************************************************************************************************************************************************
Ahri         ****************************************************************************
Jayce        *****************************************************************
...
...
...
Unchanged   
```
The other functions aren't exposed, so you would have to load up ghci:
```
$ stack exec -- env CHAMPION_GG_API_KEY=<your key> ghci
Prelude> :l src/Lib
Lib> state <- pullState "Middle"
Lib> runReader (bestAllRounder ["Fizz","LeBlanc","Annie"]) state
("Fizz",987.5759)
```
