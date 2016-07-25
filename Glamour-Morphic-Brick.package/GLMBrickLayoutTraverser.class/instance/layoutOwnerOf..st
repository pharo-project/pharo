traversing
layoutOwnerOf: aBrick

	aBrick owner ifNotNil: [ self layoutAround: aBrick owner inContext: aBrick ]