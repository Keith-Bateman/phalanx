# Phalanx
A tactical 7DRL about a Roman legionary who has his soul split in two by a witch

# Compilation
## Dependencies
* ncurses or pdcurses
* clisp
* quicklisp
## Executable
Running the "compile" script should work, although it's only been tested on my machine.

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

# Miscellaneous
Here is some information that isn't documented anywhere else except in the code
## Equipment
Equipment in Phalanx is managed in a nontraditional fashion. Everything in the inventory is treated as "equipped". However, the player only gets one attack, defense, and max hp bonus each for blue for red, so the largest bonus in either category is chosen. Some items present large bonuses, and then disappear once they are used, so the player has to question whether they want to keep the bonus or use the item
