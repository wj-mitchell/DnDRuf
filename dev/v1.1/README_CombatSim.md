# **CombatSim.R** v*1.1*

## How to Use


## Updates
### - *Headlines* -

#### Error Messaging
```CombatSim()``` will now generate descriptive error messages when you enter values it cannot understand. 

#### Simulation Seeding 
You now have the option to seed any simulation so that it can be reliably replicated and recalled in the future.    

#### Output Types
The function will now output two types of data: 1) a table containing the statistics of each fighter at the conclusion of the simulation, or 2) the play-by-play output of what happened during the simulation. 

#### Play By Play Types
The options for Play By Play output have been expanded, allowing users to only get a general summary of the battle's conclusion, only see actions relevant to deaths, only see actions relevant to deaths and attacks, or to see all actions.

#### Death Saves
When characters reach 0 hitpoints, they now automatically perform death saves, with 3 successes resulting in stablization and 3 failures resulting in death. 

#### Integration of PartySim()
All of ```PartySim()```'s recent updates have fully integrated into ```CombatSim()```

#### Integration of InitatiSim()
Initiative now mirrors that of typical tabletop gameplay, wherein players roll a d20 add DEX modifiers. using the custom ```InitiativSim()``` function I developed.

### - *Details* -

#### Error Messaging
These include errors when feeding values to the ```Output = ```, ```Ending = ```, and ```PlayByPlay = ``` arguments that R does not understand (e.g., entering ```text``` instead of ```Text``` to ```Output = ```). Additionally, R will tell you if the values entered to ```Seed = ``` and ```nRounds = ``` are non-numeric. Lastly, R will check the dataframe submitted and ensure that it is properly formatted, as specified by ```PartySim()``` (formerly known as ```FighterSim()```).

#### Simulation Seeding 
Entering any numeric value into ```Seed = ``` will determine which randomizer R will use, effectively allowing you to generate the same simulation time and time again. This is highly valuable in conjunction with the addition of new *Output Functionality* wherein you can export either the Combatant Stats from a simulation or the Turn-by-Turn Summary, but not necessarily both at the same time. By entering an arbitrary seed value that you decide upon and running the simulation twice, once set to ```Output = 'Text'``` and once set to ```Output = 'Table'```, you could have both forms of summary for the same simulation. 

#### Output Types
Both the ```Table``` and the ```Text``` outputs will take the structure of a dataframe. Unfortunately, you cannot export both datatypes simultaneously, though, hopefully this will be figured out in the future. However, with the new ```Seed = ``` argument, if you were to run the simulation twice, once set to ```Output = 'Text'``` and once set to ```Output = 'Table'```, you could have both forms of summary for the same simulation.

#### Play By Play Types
The possible values that ``` PlayByPlay = ``` can take include: 1) ```Off```, 2) ```Kills```, 3) ```Attacks```, and 4) ```Everything```. These have no effect upon ```Table``` output, but only influence what you see in ```Text``` output. 

#### Death Saves
When the for loop iterates to a character and ```LifeStatus = "Unconscious"```, the ```roll_dice()``` function from ```rollr``` will roll 1d10. If the value is 1, they take two death save failures. If the value is 20, they take two death save successes. Any values of 10 or below will result in one death save failure, and any values of 11 or above will result in one death save success. This process repeats each turn until they reach three or more death save successes or failures, at which point they will either stabilize or die, respectively. Their ```LifeStatus``` will reflect the outcome. In either case, they will be removed from the battle, however, future iterations of the function will allow conscious characters to use their turn to heal themselves using inventory health potions (*See* **```PartySim()``` v1.1 Updates**), but also, more relevant to this addition, use their action and inventory health potions to aid fallen comrades, changing them from ```LifeStatus = "Unconscious"``` to ```LifeStatus = "Alive"```. 

#### Integration of ```PartySim()```
```PartySim()``` will now generate names for characters, ability scores, and allow users to create more than two groups to battle each other. ```CombatSim()``` will use all of this information to some capacity, though that capacity will expand further in the future. *See* **```PartySim()``` v1.1 Updates** for further details.

#### Integration of ```InitatiSim()```
In order to mirror typical gameplay, I developed a custom function called ```InitiativSim()``` which has every character entered roll a d40 using ```rollr```'s ```roll_dice()``` function. Each characters ```DEX_mod``` score is added to this roll. R will then create a vector of all of the unique ```Roll``` and ```DEX_mod``` values and iterate systematically through them in a descending fashion. If more than 1 character had the same ```ROLL``` value, then R examines the ```DEX_mod``` value and awards higher initiative to whoever has the higher ```DEX_mod```. If more than one character has equivalent ```ROLL``` and ```DEX_mod``` values, initiative will be randomly assigned to these characters. It will follow these rules until every character has an initiative order.  
