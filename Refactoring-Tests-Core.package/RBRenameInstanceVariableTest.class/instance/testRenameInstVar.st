tests
testRenameInstVar
	| refactoring class |
	refactoring := RBRenameInstanceVariableRefactoring rename: 'classBlock' to: 'asdf' in: RBBasicLintRuleTest.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBBasicLintRuleTest.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self
		assert:
			(class parseTreeFor: #checkClass:)
				=
					(RBParser
						parseMethod:
							'checkClass: aSmalllintContext 
								^asdf value: aSmalllintContext value: result').
	self
		assert:
			(class parseTreeFor: #initialize)
				=
					(RBParser
						parseMethod:
							'initialize
	super initialize.
	self anInstVar: 1.
	asdf := [:context :aResult | ].
	methodBlock := [:context :aResult | ].
	self resultClass: RBSelectorEnvironment')