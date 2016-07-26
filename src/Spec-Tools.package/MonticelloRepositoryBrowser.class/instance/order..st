menu
order: anInteger

	order := anInteger.
	self workingCopies sortingBlock: (self orderSpecs at: order) value.
	self order = 4 "dirty only"
		ifTrue: [self workingCopies filteringBlock: [:col | col select: #modified]]
		ifFalse: [self workingCopies resetFilteringBlock]. 