selecting
selectNextCategory
	|currentCategory nextCategory|
	
	currentCategory := self candidates getProcessorLinkByCandidateLink: self selected.
	nextCategory := self candidates nextProcessorLinkOf: currentCategory.
	
	self selected: ((currentCategory = nextCategory)
		ifTrue: [ nextCategory tailLink ]
		ifFalse: [ nextCategory headLink ])
	
	