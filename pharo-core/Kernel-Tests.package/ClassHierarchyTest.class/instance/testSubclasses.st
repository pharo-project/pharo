tests
testSubclasses
	" self class fixSubclasses "
	
	| subclasses |
	
	SystemNavigation new allClassesDo: [ :cls|
		self 
			assert: (cls superclass subclasses includes: cls) 
			description: cls name, ' is not in ', cls superclass name, '''s subclasses' ]