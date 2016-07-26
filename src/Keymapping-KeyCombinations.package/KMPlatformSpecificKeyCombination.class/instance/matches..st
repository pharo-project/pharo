testing
matches: aCollection

	self platform == Smalltalk os platformFamily ifFalse: [ ^ false ].	
	^ self shortcut matches: aCollection