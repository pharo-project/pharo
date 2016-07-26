tests
testPushDownMethod
	| refactoring class |
	refactoring := RBPushDownMethodRefactoring 
		pushDown: #(#name: )
		from: RBLintRuleTest.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTest.
	self deny: (class directlyDefinesMethod: #name:).
	class subclasses do: 
		[ :each | 
		self assert: (each parseTreeFor: #name:) = (RBParser parseMethod: 'name: aString name := aString') ]