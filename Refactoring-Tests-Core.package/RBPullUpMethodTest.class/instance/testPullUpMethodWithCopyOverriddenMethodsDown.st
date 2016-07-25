tests
testPullUpMethodWithCopyOverriddenMethodsDown
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpMethodRefactoring 
			pullUp: #(#isComposite )
			from: RBCompositeLintRuleTest.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTest) parseTreeFor: #isComposite) = (RBParser parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: ('RBFoo' , 'LintRuleTest') asSymbol) parseTreeFor: #isComposite) = (RBParser parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: #RBLintRuleTest) parseTreeFor: #isComposite) = (RBParser parseMethod: 'isComposite ^true').
	self deny: ((refactoring model classNamed: #RBCompositeLintRuleTest) directlyDefinesMethod: #isComposite)