code generation (closures)
computeCopiedValues: rootNode
	| referencedValues |
	referencedValues := rootNode referencedValuesWithinBlockExtent: blockExtent.
	^(referencedValues reject: [:temp| temp isDefinedWithinBlockExtent: blockExtent])
		asArray sort: ParseNode tempSortBlock