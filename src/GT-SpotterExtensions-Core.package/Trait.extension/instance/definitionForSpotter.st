*GT-SpotterExtensions-Core
definitionForSpotter
	| str |
	str := WriteStream on: String new.
	str nextPutAll: self definitionForNautilus.
	str cr; cr.
	str nextPut: $".
	str nextPutAll: 'Hierarchy: '.
	str nextPutAll: ($> join: self allSuperclasses reversed).
	str cr; cr.
	self hasComment ifTrue: [ 
		str nextPutAll: self comment.
	].	
	str nextPut: $".
	^ str contents