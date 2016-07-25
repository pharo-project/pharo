replacing
replaceNode: aNode withNode: anotherNode 
	body == aNode ifTrue: [self body: anotherNode].
	self arguments: (arguments 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])