validation
withChildrenOf: anObject do: aBlock
	aBlock value: anObject.
	(self childrenValueOf: anObject) do: [ :each |
		self withChildrenOf: each do: aBlock ]