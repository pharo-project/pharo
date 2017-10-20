accessing
expectedBehavior
	| result |
	
	^result := { 
		[Symbol readFromString: '#''abc'''] -> #abc.
		[Symbol readFromString: '#ab-C'] -> #ab.
	 	[Symbol readFromString: '#abC'] -> #abC. 
		[Symbol readFromString: '#ab@c'] -> #ab}.