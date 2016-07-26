menu
addNavigationMenuItemsTo: aMenu
	aMenu	add: 'Browse' target: self selector: #browseCritic.
		
	rule isArchitecturalRule ifFalse: [ 
		aMenu
			add: 'Senders' target: self selector: #sendersOfCritic;
			add: 'Implementors' target: self selector: #implementorsOfCritic  ]