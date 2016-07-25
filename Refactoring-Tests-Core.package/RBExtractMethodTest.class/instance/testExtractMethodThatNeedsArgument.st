tests
testExtractMethodThatNeedsArgument
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (145 to: 343)
				for: (RBTransformationRuleTest sourceCodeAt: #checkMethod:))
		from: #checkMethod:
		in: RBTransformationRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext 
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[self foo: aSmalllintContext]').
	self assert: (class parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: aSmalllintContext (RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse: 
					[builder compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]')