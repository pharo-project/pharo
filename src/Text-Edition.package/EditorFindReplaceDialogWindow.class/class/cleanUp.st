initializing
cleanUp
	Singleton ifNotNil: [ 
			Singleton close.
			Singleton := nil ]