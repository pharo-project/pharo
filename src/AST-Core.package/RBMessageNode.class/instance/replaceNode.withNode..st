replacing
replaceNode: aNode withNode: anotherNode 
	"If we're inside a cascade node and are changing the receiver, change all the receivers"

	receiver == aNode 
		ifTrue: 
			[self receiver: anotherNode.
			(parent notNil and: [parent isCascade]) 
				ifTrue: [parent messages do: [:each | each receiver: anotherNode]]].
	self arguments: (arguments 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])