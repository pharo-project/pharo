tests
testPullUpInstVar
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpInstanceVariableRefactoring 
			variable: 'result'
			class: RBLintRuleTest.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBLintRuleTest) directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBBasicLintRuleTest) directlyDefinesInstanceVariable: 'result')