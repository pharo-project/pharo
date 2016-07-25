testing
overridesDefaultSetOrTearMethods
	
	^ self overridesDefaultTearDownMethod or: [ 
		self overridesDefaultSetUpMethod ]