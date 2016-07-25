private
continueFilter
	| processors nonEmptyProcessors |
	processors := self cache at: #processorsAndFilters ifAbsent: [ ^ self ].
	nonEmptyProcessors := processors associations select: [ :assoc | assoc key hasFilteredCandidates ].
	nonEmptyProcessors size = 1 ifFalse: [ ^ self ].
	self canContinue ifFalse: [ ^ self ].
	
	self withContinue: true do: [ 
		nonEmptyProcessors do: [ :assoc | 
			assoc key continueFilterInContext: self ] ]