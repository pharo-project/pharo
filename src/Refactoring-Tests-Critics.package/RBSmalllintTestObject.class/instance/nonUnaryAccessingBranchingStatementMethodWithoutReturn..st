accessing - good
nonUnaryAccessingBranchingStatementMethodWithoutReturn: anObject
	"shouldn't trigger the RBUnaryAccessingMethodWithoutReturnRule rule"

	self value isString
		ifTrue: [ self value ]
		ifFalse: [ '' ]