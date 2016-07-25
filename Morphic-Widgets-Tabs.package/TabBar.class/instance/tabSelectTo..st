private-actions
tabSelectTo: aTab
	| index selectedIndex |
	
	index := tabs indexOf: aTab. "If absent, then the system must be in a curious shape"
	selectedIndex := tabs indexOf: self selectedTab ifAbsent: [ ^ self ]. "No currently selected tab"
	
	index compareWith: selectedIndex
		ifLesser: [ 
			selectedIndex to: index by: -1 do: [ :i | 
				(tabs at: i) addToSelection	] ]
		ifEqual: [  ]
		ifGreater: [ 
			selectedIndex to: index do: [ :i | 
				(tabs at: i) addToSelection	] ].
			
	self addInHistory: aTab.
	self adjustLayout.