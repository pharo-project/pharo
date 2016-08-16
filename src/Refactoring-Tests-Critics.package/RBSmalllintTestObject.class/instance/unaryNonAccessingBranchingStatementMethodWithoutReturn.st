non-accessing
unaryNonAccessingBranchingStatementMethodWithoutReturn
	"shouldn't trigger the RBUnaryAccessingMethodWithoutReturnRule rule"

	self value isString
		ifTrue: [ self value ]
		ifFalse: [ '' ]