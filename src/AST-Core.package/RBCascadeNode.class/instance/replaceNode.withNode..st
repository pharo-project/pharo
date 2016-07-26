replacing
replaceNode: aNode withNode: anotherNode 
	self messages: (messages 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])