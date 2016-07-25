failure tests
testPushDownMethodOnNonAbstractClass
	| refactoring |
	refactoring := RBPushDownMethodRefactoring pushDown: #(#isArray) from: Array.
	self shouldFail: refactoring