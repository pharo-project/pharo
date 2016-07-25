private
handleReturnKey
	| answer |
	answer := RubReturnEntered morph: self.
	self announce: answer.
	answer accepted
		ifTrue: [ self editor textWasAccepted ].
	^ answer accepted