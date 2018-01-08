# Phalanx
A tactical 7DRL about a Roman legionary who has his soul split in two by a witch

# Compilation
## Dependencies
* ncurses or pdcurses
* clisp
* quicklisp
## Creating an Executable
Running the "compile" script should work, although it's only been tested on my machine.
## Provided Executable
The provided executable was compiled on an x86_64 machine running Ubuntu GNU/Linux
NOTE: I would like to provide native executables for other architectures if possible,
and assistance in this endeavor would be appreciated.

# Controls
## Directional
hjkl - left down up right
yubn - diagonal (up left) (up right) (down left) (down right)
## Formational
These are commands unique to this game
sdfg - separate counter clockwise gather
Separate causes your units to move away from each other
Clockwise and counter cause your units to rotate around in a rectangle
Gather causes your units to move toward each other
## Logistical
i - inventory
, - pickup
; - look
m - menu
## Inventory
The inventory is where all items are used from. This is meant to be something of a coffeebreak roguelike, so I wanted to keep the controls as transparent as possible. The commands for the inventory are displayed when you go to the inventory, but I put them here for reference.

t - transfer
d - drop
a - apply
q - quit

Transfer means move the item from one inventory to the other (since you control two @s, you get two inventories)
Apply means use the item
Quit quits out of the inventory, not the game
## Menu
This is the main menu screen that you go to when you start the game. You can navigate it by moving the cursor with hjkl and pressing enter or space when you get to the option you want or by pressing the upper or lower case letter of the desired option, so there's really no way to mess it up.
I should point out that Continue Saved Game continues a game that has been saved with Save Game, while Resume Game continues the current game, and Quit in this case quits the game, not the menu. Note that Quit will delete your save file without warning. Use save to save and quit.
## Look
I couldn't resist including a Lisp data structure in game when it came to the look command. The look command presents all the items on a tile as a Lisp list. If there isn't anything on the tile, it shows "NIL". I thought about formatting this differently, but in the end I liked it enough to keep it.

# Miscellaneous
Here is some information that isn't documented anywhere else except in the code
## Stairs
In order to use stairs you need to have both red and blue players on a proper staircase and press ">" or "<" (depending on the staircase)
## Equipment
Equipment in Phalanx is managed in a nontraditional fashion. Everything in the inventory is treated as "equipped". However, the player only gets one attack, defense, and max hp bonus each for blue and for red, so the largest bonus in either category is chosen. Some items present large bonuses, and then disappear once they are used, so the player has to question whether they want to keep the bonus or use the item. Also, some items can break without warning. Not all items have uses, but the ones that do usually put up a message of some sort after they've been used.
## Leaders
Capital letters are all leaders. They attract nearby monsters to them and generally have higher stats, but they give more experience.
## Bugs
These are extremely numerous. One particularly nasty bug is the slowdown experienced on later levels. I have ideas about how to fix many bugs, but most will have to wait for a bugfix release after the 7 days I have allotted for this project. Please email me at kbateman@hawk.iit.edu with anything serious.
### Balance issues
The game is pretty imbalanced. At first, it was too hard, so I spent a lot of time making it easier so that people could beat it without too much trouble, and now it's too easy. I stuck with it because I would rather the game were too easy than too difficult.

# Credits
Well, I did all the coding myself (although some of the algorithms were taken from another project of mine in order to save time), and the idea was mine, but I had a lot of help from outside. RogueBasin provided lots of algorithms, and I even took one algorithm from AI Memo 239 (HAKMEM). I used the curses bindings from Timofei Shatrov's "The Sewer Massacre" and took some inspiration from the "Complete Roguelike Tutorial, using python+libtcod" which can be found on RogueBasin. Of course I took lots of basic ideas from other roguelikes like NetHack and Angband.