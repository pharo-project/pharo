A DockingBarToggleMenuItemMorph is a specialized version of its superclass for DockingBar behavior.

There is a bit of duplication between 
DockingBarToggleMenuItemMorph and DockingBarMenuItemMorph because iof single inheritance limit: 

DockingBarToggleMenuItemMorph inherits from ToggleMenuItemMorph (and also from DockingBarMenuItemMorph).

One of the problem is that in addition, the use of toggleMenuItem or simple menuItem looks random.

DockingBarMenuItemMorph is created by
	add: wordingString icon: aForm help: helpString subMenu: aMenuMorph 
	
and DockingBarToggleMenuItemMorph is created by
	add: wordingString font: aFont icon: aForm help: helpString subMenu: aMenuMorph 
