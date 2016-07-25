protocol
accept: aText notifying: aController

	(self isClassDefinitionExpression: aText)
		ifFalse: [ ^ self notifyNoClassDefintion ].
		
	self model compileAClassFrom: aText notifying: aController