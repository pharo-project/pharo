tests
testInlineMethodWithMultipleSendersInMethod
	| refactoring |
	refactoring := RBInlineAllSendersRefactoring 
		sendersOf: #caller2
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineLast) = (RBParser parseMethod: 'inlineLast
	5 = 3 ifTrue: [^self caller] ifFalse: [^(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]] ').
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #multipleCalls) = (RBParser parseMethod: 'multipleCalls
	(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)].
	(1 to: 10) inject: 1 into: [:sum1 :each1 | sum1 * (self foo: each1)]')