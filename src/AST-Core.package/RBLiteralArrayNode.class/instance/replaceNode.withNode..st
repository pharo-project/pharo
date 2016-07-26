replacing
replaceNode: aNode withNode: anotherNode 
	self contents: (contents 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])