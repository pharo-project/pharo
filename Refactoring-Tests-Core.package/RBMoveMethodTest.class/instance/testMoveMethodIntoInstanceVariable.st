tests
testMoveMethodIntoInstanceVariable
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #checkMethod:
			class: RBTransformationRuleTest
			variable: 'rewriteRule'.
		self 
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self 
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: #RBParseTreeRewriter)).
		self 
			setupMethodNameFor: refactoring
			toReturn: #foo:foo:
			withArguments: #('transformationRule' 'aSmalllintContext' ).
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext rewriteRule foo: self foo: aSmalllintContext').
	self assert: ((refactoring model classNamed: #RBParseTreeRewriter) parseTreeFor: #foo:foo:) = (RBParser parseMethod: 'foo: transformationRule foo: aSmalllintContext
	transformationRule class: aSmalllintContext selectedClass.
	(self executeTree: aSmalllintContext parseTree) ifTrue: 
			[(transformationRule class recursiveSelfRule executeTree: self tree initialAnswer: false)
				ifFalse: 
					[transformationRule builder compile: self tree printString
						in: transformationRule class1
						classified: aSmalllintContext protocols]]').
	self assert: (class parseTreeFor: #class1) = (RBParser parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) = (RBParser parseMethod: 'class: anObject class := anObject').
	self assert: (class theMetaClass parseTreeFor: #recursiveSelfRule:) = (RBParser parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (class theMetaClass parseTreeFor: #recursiveSelfRule) = (RBParser parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeFor: #builder) = (RBParser parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) = (RBParser parseMethod: 'builder: anObject builder := anObject')