initialize
initializePresenter
	self wirePackages.
	self wireClasses.
	self wireProtocols.
	self wireSelectors.
	
	showClassSide 
		whenChangedDo: [ self refreshClasses: self packages selectedItem ].