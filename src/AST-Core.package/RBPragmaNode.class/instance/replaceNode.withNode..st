replacing
replaceNode: aNode withNode: anotherNode 
	self arguments: (arguments collect: [ :each | 
		each == aNode 
			ifTrue: [ anotherNode ]
			ifFalse: [ each ] ])