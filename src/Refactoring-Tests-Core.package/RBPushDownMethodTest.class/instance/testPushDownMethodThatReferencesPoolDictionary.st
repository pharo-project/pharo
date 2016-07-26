tests
testPushDownMethodThatReferencesPoolDictionary
	| refactoring class parseTree |
	parseTree := RBLintRuleTest parseTreeFor: #junk.
	self proceedThroughWarning: 
		[ refactoring := RBPushDownMethodRefactoring 
			pushDown: #(#junk )
			from: RBLintRuleTest.
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTest.
	self deny: (class directlyDefinesMethod: #junk).
	class subclasses do: 
		[ :each | 
		self assert: (each parseTreeFor: #junk) = parseTree.
		self assert: (each directlyDefinesPoolDictionary: 'TextConstants' asSymbol) ]