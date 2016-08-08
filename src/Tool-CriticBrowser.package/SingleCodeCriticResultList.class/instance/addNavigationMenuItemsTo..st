menu
addNavigationMenuItemsTo: aMenu
	aMenu	add: 'Browse' target: self selector: #browseCritic.
		
	aMenu
		add: 'Senders' target: self selector: #sendersOfCritic;
		add: 'Implementors' target: self selector: #implementorsOfCritic