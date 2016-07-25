tests
testTemporaryToInstanceVariable
	| refactoring class |
	refactoring := RBTemporaryToInstanceVariableRefactoring 
		class: RBLintRuleTest
		selector: #displayName
		variable: 'nameStream'.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class parseTreeFor: #displayName) = (RBParser parseMethod: 'displayName
								nameStream := WriteStream on: (String new: 64).
								nameStream
									nextPutAll: self name;
									nextPutAll: '' (''.
								self problemCount printOn: nameStream.
								nameStream nextPut: $).
								^nameStream contents').
	self assert: (class directlyDefinesInstanceVariable: 'nameStream')