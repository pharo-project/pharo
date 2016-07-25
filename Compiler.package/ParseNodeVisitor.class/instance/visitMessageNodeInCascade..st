visiting
visitMessageNodeInCascade: aMessageNode
	"receiver is nil for cascades"
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]