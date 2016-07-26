accessing decorators
withDecoratorsNamed: aCollection
	aCollection do: [ :m | self withDecoratorNamed: m ]