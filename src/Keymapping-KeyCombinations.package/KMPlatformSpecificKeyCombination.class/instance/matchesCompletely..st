testing
matchesCompletely: aCollection

	self platform == Smalltalk os platformFamily ifFalse: [ ^ false ].	
	^ self shortcut matchesCompletely: aCollection