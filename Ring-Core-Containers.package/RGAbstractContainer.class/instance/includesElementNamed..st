testing
includesElementNamed: elementName

	^ elements anySatisfy: [ :collection | self includesElementNamed: elementName asSymbol in: collection ]