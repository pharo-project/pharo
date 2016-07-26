accessing decorators
withoutDecoratorsNamed: aCollection
	aCollection do: [ :m | self withoutDecoratorNamed: m ]