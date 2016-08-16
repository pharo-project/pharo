accessing - bad
unaryAccessingBranchingStatementMethodWithoutReturn
	"should trigger the RBUnaryAccessingMethodWithoutReturnRule rule"

	self value isString
		ifTrue: [ self value ]
		ifFalse: [ '' ]