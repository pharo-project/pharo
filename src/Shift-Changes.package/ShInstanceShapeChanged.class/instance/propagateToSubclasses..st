propagating
propagateToSubclasses: anotherBuilder
	anotherBuilder
		addChange:
			(self class new
				builder: anotherBuilder;
				yourself)