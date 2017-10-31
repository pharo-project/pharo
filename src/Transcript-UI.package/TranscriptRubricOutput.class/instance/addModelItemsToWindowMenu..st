window
addModelItemsToWindowMenu: aMenu 

	RubricOutputMenuBuilder new 
		setModel: self;
		submenuFor: aMenu.