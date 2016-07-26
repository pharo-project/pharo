tests
testAbstractInstanceVariable
	| refactoring class |
	refactoring := RBAbstractInstanceVariableRefactoring 
		variable: 'class'
		class: RBTransformationRuleTest.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #class1) = (RBParser parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) = (RBParser parseMethod: 'class: anObject
	class := anObject').
	self assert: (class parseTreeFor: #superSends) = (RBParser parseMethod: 'superSends
	| rule |
	rule := RBParseTreeRewriter new.
	rule addSearch: ''super `@message: ``@args'' 
				-> (
					[:aNode | 
					(self class1 withAllSubclasses 
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil] 
							-> ''self `@message: ``@args'').
	self rewriteUsing: rule').
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext 
	self class: aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) 
		ifTrue: 
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) 
				ifFalse: 
					[builder 
						compile: rewriteRule tree printString
						in: self class1
						classified: aSmalllintContext protocols]]')