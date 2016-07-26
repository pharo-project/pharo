testing
hasArgumentNamed: aString	
	^ self arguments anySatisfy: [ :argument| argument name = aString ]