assertions
should: aBlock enumerate: aFirstCollection and: aSecondCollection
	| firstEnumerated secondEnumerated |

	firstEnumerated := OrderedCollection new.
	secondEnumerated := OrderedCollection new.
	aBlock value: [:first :second |
		firstEnumerated addLast: first.
		secondEnumerated addLast: second].
	self
		assert: firstEnumerated = aFirstCollection asOrderedCollection;
		assert: secondEnumerated = aSecondCollection asOrderedCollection.