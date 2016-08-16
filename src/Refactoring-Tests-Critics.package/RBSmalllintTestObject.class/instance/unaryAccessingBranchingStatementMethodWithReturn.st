accessing - good
unaryAccessingBranchingStatementMethodWithReturn
	"shouldn't trigger the RBUnaryAccessingMethodWithoutReturnRule rule"

	self value isString
		ifTrue: [ ^ self value ]
		ifFalse: [ ^ '' ]