visiting
visitMessageNode: aMessageNode
	aMessageNode receiver accept: self.
	"receiver notNil ifTrue: ''receiver is nil for cascades''
		[receiver accept: self]."
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]