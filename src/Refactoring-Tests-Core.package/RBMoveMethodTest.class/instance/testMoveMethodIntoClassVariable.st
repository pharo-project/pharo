tests
testMoveMethodIntoClassVariable
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #checkMethod:
			class: RBTransformationRuleTest
			variable: 'RecursiveSelfRule'.
		self 
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self 
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: #RBParseTreeSearcher)).
		self 
			setupMethodNameFor: refactoring
			toReturn: #foo:foo:
			withArguments: #('transformationRule' 'aSmalllintContext' ).
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext RecursiveSelfRule foo: self foo: aSmalllintContext').
	self assert: ((refactoring model classNamed: #RBParseTreeSearcher) parseTreeFor: #foo:foo:) = (RBParser parseMethod: 'foo: transformationRule foo: aSmalllintContext
	transformationRule class: aSmalllintContext selectedClass.
	(transformationRule rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[(self executeTree: transformationRule rewriteRule tree initialAnswer: false)
				ifFalse: 
					[transformationRule builder compile: transformationRule rewriteRule tree printString
						in: transformationRule class1
						classified: aSmalllintContext protocols]]').
	self assert: (class parseTreeFor: #class1) = (RBParser parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) = (RBParser parseMethod: 'class: anObject class := anObject').
	self assert: (class parseTreeFor: #builder) = (RBParser parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) = (RBParser parseMethod: 'builder: anObject builder := anObject').
	self assert: (class parseTreeFor: #rewriteRule) = (RBParser parseMethod: 'rewriteRule ^rewriteRule').
	self assert: (class parseTreeFor: #rewriteRule:) = (RBParser parseMethod: 'rewriteRule: anObject rewriteRule := anObject')