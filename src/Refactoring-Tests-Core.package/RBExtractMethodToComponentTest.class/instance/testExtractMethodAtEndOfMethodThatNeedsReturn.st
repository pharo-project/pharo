tests
testExtractMethodAtEndOfMethodThatNeedsReturn
	| refactoring class selectorsSize |
	refactoring := RBExtractMethodToComponentRefactoring 
		extract: (self 
				convertInterval: (52 to: 133)
				for: (RBLintRuleTest sourceCodeAt: #openEditor))
		from: #openEditor
		in: RBLintRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self 
		setupSelfArgumentNameFor: refactoring
		toReturn: 'asdf'.
	self 
		setupVariableToMoveToFor: refactoring
		toReturn: 'rules'.
	self 
		setupVariableTypesFor: refactoring
		toReturn: (Array with: (refactoring model classNamed: #Collection)).
	class := refactoring model classNamed: #RBLintRuleTest.
	selectorsSize := class selectors size.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self assert: (class parseTreeFor: #openEditor) = (RBParser parseMethod: 'openEditor
								| rules |
								rules := self failedRules.
								^rules foo: self').
	self assert: ((refactoring model classNamed: #Collection) parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: asdf
								self isEmpty ifTrue: [^asdf].
								self size == 1 ifTrue: [^self first viewResults].
								^asdf').
	self assert: class selectors size = selectorsSize