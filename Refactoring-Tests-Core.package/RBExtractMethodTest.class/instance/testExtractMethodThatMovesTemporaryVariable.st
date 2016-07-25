tests
testExtractMethodThatMovesTemporaryVariable
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (22 to: 280)
				for: (RBTransformationRuleTest sourceCodeAt: #superSends))
		from: #superSends
		in: RBTransformationRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo1.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #superSends) = (RBParser parseMethod: 'superSends
	| rule |
	rule := self foo1.
	self rewriteUsing: rule').
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 | rule | 	rule := RBParseTreeRewriter new.
	rule addSearch: ''super `@message: ``@args''
				-> (
					[:aNode | 
					(class withAllSubclasses
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil]
							-> ''self `@message: ``@args'').
		^rule')