Class {
	#name : #RBMoveMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBMoveMethodTest >> testMoveMethodIntoArgument [
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #checkMethod:
			class: RBTransformationRuleTestData
			variable: 'aSmalllintContext'.
		self 
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self 
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: #RBSmalllintContext)).
		self 
			setupMethodNameFor: refactoring
			toReturn: #foo:.
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class parseTreeFor: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext aSmalllintContext foo: self').
	self assert: ((refactoring model classNamed: #RBSmalllintContext) parseTreeFor: #foo:) equals: (self parseMethod: 'foo: transformationRule
	transformationRule class: self selectedClass.
	(transformationRule rewriteRule executeTree: self parseTree) ifTrue: 
			[(transformationRule class recursiveSelfRule executeTree: transformationRule rewriteRule tree initialAnswer: false)
				ifFalse: 
					[transformationRule builder compile: transformationRule rewriteRule tree printString
						in: transformationRule class1
						classified: self protocols]]').
	self assert: (class parseTreeFor: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) equals: (self parseMethod: 'class: anObject class := anObject').
	self assert: (class classSide parseTreeFor: #recursiveSelfRule:) equals: (self parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (class classSide parseTreeFor: #recursiveSelfRule) equals: (self parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeFor: #builder) equals: (self parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) equals: (self parseMethod: 'builder: anObject builder := anObject').
	self assert: (class parseTreeFor: #rewriteRule) equals: (self parseMethod: 'rewriteRule ^rewriteRule').
	self assert: (class parseTreeFor: #rewriteRule:) equals: (self parseMethod: 'rewriteRule: anObject rewriteRule := anObject')
]

{ #category : #tests }
RBMoveMethodTest >> testMoveMethodIntoClassVariable [
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #checkMethod:
			class: RBTransformationRuleTestData
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
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class parseTreeFor: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext RecursiveSelfRule foo: self foo: aSmalllintContext').
	self assert: ((refactoring model classNamed: #RBParseTreeSearcher) parseTreeFor: #foo:foo:) equals: (self parseMethod: 'foo: transformationRule foo: aSmalllintContext
	transformationRule class: aSmalllintContext selectedClass.
	(transformationRule rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[(self executeTree: transformationRule rewriteRule tree initialAnswer: false)
				ifFalse: 
					[transformationRule builder compile: transformationRule rewriteRule tree printString
						in: transformationRule class1
						classified: aSmalllintContext protocols]]').
	self assert: (class parseTreeFor: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) equals: (self parseMethod: 'class: anObject class := anObject').
	self assert: (class parseTreeFor: #builder) equals: (self parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) equals: (self parseMethod: 'builder: anObject builder := anObject').
	self assert: (class parseTreeFor: #rewriteRule) equals: (self parseMethod: 'rewriteRule ^rewriteRule').
	self assert: (class parseTreeFor: #rewriteRule:) equals: (self parseMethod: 'rewriteRule: anObject rewriteRule := anObject')
]

{ #category : #tests }
RBMoveMethodTest >> testMoveMethodIntoInstanceVariable [
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #checkMethod:
			class: RBTransformationRuleTestData
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
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class parseTreeFor: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext rewriteRule foo: self foo: aSmalllintContext').
	self assert: ((refactoring model classNamed: #RBParseTreeRewriter) parseTreeFor: #foo:foo:) equals: (self parseMethod: 'foo: transformationRule foo: aSmalllintContext
	transformationRule class: aSmalllintContext selectedClass.
	(self executeTree: aSmalllintContext parseTree) ifTrue: 
			[(transformationRule class recursiveSelfRule executeTree: self tree initialAnswer: false)
				ifFalse: 
					[transformationRule builder compile: self tree printString
						in: transformationRule class1
						classified: aSmalllintContext protocols]]').
	self assert: (class parseTreeFor: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) equals: (self parseMethod: 'class: anObject class := anObject').
	self assert: (class classSide parseTreeFor: #recursiveSelfRule:) equals: (self parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (class classSide parseTreeFor: #recursiveSelfRule) equals: (self parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeFor: #builder) equals: (self parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) equals: (self parseMethod: 'builder: anObject builder := anObject')
]

{ #category : #tests }
RBMoveMethodTest >> testMoveMethodThatReferencesPoolDictionary [
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #junk
			class: RBLintRuleTestData
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
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeFor: #junk) equals: (self parseMethod: 'junk ^RefactoryTestDataApp junk1').
	self assert: ((refactoring model metaclassNamed: #RBRefactoryTestDataApp) parseTreeFor: #junk1) equals: (self parseMethod: 'junk1
	^RBRefactoryTestDataApp printString copyFrom: 1 to: CR').
	self assert: (class directlyDefinesPoolDictionary: 'TextConstants' asSymbol)
]

{ #category : #'failure tests' }
RBMoveMethodTest >> testMovePrimitiveMethod [
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := RBMoveMethodRefactoring 
				model: model
				selector: #foo
				class: Object
				variable: 'OrderedCollection'.
	self shouldFail: refactoring
]
