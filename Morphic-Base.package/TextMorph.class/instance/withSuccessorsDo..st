linked frames
withSuccessorsDo: aBlock 
	"Evaluate aBlock for each morph in my successor chain"

	| each |
	each := self.
	[each isNil] whileFalse: 
			[aBlock value: each.
			each := each successor]