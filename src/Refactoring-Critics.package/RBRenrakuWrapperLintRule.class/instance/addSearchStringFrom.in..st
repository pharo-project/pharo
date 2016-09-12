adding
addSearchStringFrom: aCritique in: aSourceCode

	aCritique sourceAnchor providesInterval ifTrue: [
		| interval |
		interval := aCritique sourceAnchor interval.
		result addSearchString: (
			aSourceCode
				copyFrom: interval first
				to: interval last)  ].