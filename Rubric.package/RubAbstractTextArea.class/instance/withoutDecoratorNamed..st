accessing decorators
withoutDecoratorNamed: aKey
	(self decoratorNamed: aKey)
		ifNotNil: [ :m | self withoutDecorator: m ].
	self changed