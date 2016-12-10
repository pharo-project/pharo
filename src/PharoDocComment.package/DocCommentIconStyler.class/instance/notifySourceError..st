private
notifySourceError: aNode
	"If a code in an example is faulty notify the user"
	"kjkl>>>2"
	"3 + 4 >>> 7"
	^ [ | examples faultyExample |
		examples := aNode pharoDocCommentNodes.
		faultyExample := examples detect: [ :example | example expression expressionCode isFaulty ].
		RBParser parseExpression: faultyExample expression expression ]