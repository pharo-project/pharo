protocol
tabs: aCollection

	tabs do: [ :e | e silentlySelected: false ].
	tabs := aCollection.
	selectionHistory removeAll.
	
	aCollection do: [ :aTab |
		self registerActionsFor: aTab.
		self selectedTab ifNil: [ aTab selected: true ].
		self addFirstInHistory: aTab ].
	
	self adjustLayout.
	self changed.
	self triggerEvent: #tabsChanged