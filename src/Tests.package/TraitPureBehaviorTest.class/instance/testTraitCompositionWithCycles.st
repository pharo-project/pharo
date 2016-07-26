testing-applying trait composition
testTraitCompositionWithCycles
	self should: [self t1 setTraitComposition: self t1 asTraitComposition]
		raise: Error.
	self t2 setTraitComposition: self t3 asTraitComposition.
	self should: [self t3 setTraitComposition: self t5 asTraitComposition]
		raise: Error