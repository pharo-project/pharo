initialization
initializePresenter
	createButton action: [ self createRepository ].
	changeLocationButton action: [ self changeLocation ]