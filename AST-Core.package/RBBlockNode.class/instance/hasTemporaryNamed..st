testing
hasTemporaryNamed: aString	
	^ self temporaries anySatisfy: [ :temp| temp name = aString ]