tests
testExtractMethodAtEndOfMethodThatNeedsReturn
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (52 to: 133)
				for: (RBLintRuleTest sourceCodeAt: #openEditor))
		from: #openEditor
		in: RBLintRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class parseTreeFor: #openEditor) = (RBParser parseMethod: 'openEditor
	| rules |
	rules := self failedRules.
	^self foo: rules').
	self assert: (class parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: rules
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]')