tests
testRenameInstVarNotAccessors
	| refactoring class |
	refactoring := RBRenameInstanceVariableRefactoring rename: 'result' to: 'whatever' in: RBBasicLintRuleTest renameAccessors: false.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBBasicLintRuleTest.
	self assert: (class directlyDefinesInstanceVariable: 'whatever').
	self deny: (class directlyDefinesInstanceVariable: 'result').
	
	self deny: (class definesMethod: #whatever).
	self deny: (class definesMethod: #whatever:).
	self assert: (class definesMethod: #result).
