accessing decorators
withDecoratorNamed: aKey
	(self hasDecoratorNamed: aKey)
		ifTrue: [ ^ self ].
	(self classOfDecoratorNamed: aKey)
		ifNotNil: [ :cls | self withDecorator: cls new ]