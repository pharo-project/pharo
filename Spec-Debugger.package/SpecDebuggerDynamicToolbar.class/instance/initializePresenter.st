initialization
initializePresenter
	super initializePresenter.
	
	sessionHolder whenChangedDo: [
		self spec ifNotNil: [ 
			self update	 ] ]
