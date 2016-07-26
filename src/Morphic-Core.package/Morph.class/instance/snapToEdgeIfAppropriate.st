menus
snapToEdgeIfAppropriate
	| edgeSymbol oldBounds aWorld |
	(edgeSymbol := self valueOfProperty: #edgeToAdhereTo) ifNotNil:
		[oldBounds := bounds.
		self adhereToEdge: edgeSymbol.
		bounds ~= oldBounds ifTrue: [(aWorld := self world) ifNotNil: [aWorld viewBox ifNotNil:
			[aWorld displayWorld]]]]