protocol
at: index ifAbsentPut: block

	| result |
	result := value at: index ifAbsentPut: block.
	self valueAdded: block value.
	^ result