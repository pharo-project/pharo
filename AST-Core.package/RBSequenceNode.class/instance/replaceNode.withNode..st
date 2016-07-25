replacing
replaceNode: aNode withNode: anotherNode 
	self statements: (statements 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]]).
	self temporaries: (temporaries 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])