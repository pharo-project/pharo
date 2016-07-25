tests
testExtractMethodThatNeedsTemporaryVariable
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (78 to: 197)
				for: (RBLintRuleTest sourceCodeAt: #displayName))
		from: #displayName
		in: RBLintRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class parseTreeFor: #displayName) = (RBParser parseMethod: 'displayName
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	self foo: nameStream.
	^nameStream contents').
	self assert: (class parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: nameStream 	nameStream nextPutAll: self name;
		nextPutAll: '' (''.
	self problemCount printOn: nameStream.
	nameStream nextPut: $).')