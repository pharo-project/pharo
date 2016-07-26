tests
testMoveMethodThatReferencesPoolDictionary
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #junk
			class: RBLintRuleTest
			variable: 'RefactoryTestDataApp'.
		self 
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self 
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: 'RBRefactoryTestDataApp class' asSymbol)).
		self 
			setupMethodNameFor: refactoring
			toReturn: #junk1.
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class parseTreeFor: #junk) = (RBParser parseMethod: 'junk ^RefactoryTestDataApp junk1').
	self assert: ((refactoring model metaclassNamed: #RBRefactoryTestDataApp) parseTreeFor: #junk1) = (RBParser parseMethod: 'junk1
	^RBRefactoryTestDataApp printString copyFrom: 1 to: CR').
	self assert: (class directlyDefinesPoolDictionary: 'TextConstants' asSymbol)