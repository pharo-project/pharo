Class {
	#name : 'RBMoveMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBMoveMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveMethodRefactoring };
		addCase: { #rbClass -> RBMoveMethodTransformation };
		yourself
]

{ #category : 'accessing' }
RBMoveMethodParametrizedTest >> constructor [
	^ #selector:class:variable:
]

{ #category : 'failure tests' }
RBMoveMethodParametrizedTest >> testFailureMovePrimitiveMethod [

	| refactoring |
	(model classNamed: #Object)
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #( #accessing ).
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               #foo.
				               Object.
				               'OrderedCollection' }.
	self shouldFail: refactoring
]

{ #category : 'tests' }
RBMoveMethodParametrizedTest >> testMoveMethodIntoArgument [
	| refactoring class |
	self proceedThroughWarning:
		[ refactoring := self createRefactoringWithArguments:
				{ #checkMethod: . RBTransformationRuleTestData . 'aSmalllintContext' }.
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
	self assert: (class parseTreeForSelector: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext aSmalllintContext foo: self').
	self assert: ((refactoring model classNamed: #RBSmalllintContext) parseTreeForSelector: #foo:) equals: (self parseMethod: 'foo: transformationRule
	transformationRule class: self selectedClass.
	(transformationRule rewriteRule executeTree: self parseTree) ifTrue:
			[(transformationRule class recursiveSelfRule executeTree: transformationRule rewriteRule tree initialAnswer: false)
				ifFalse:
					[transformationRule builder compile: transformationRule rewriteRule tree printString
						in: transformationRule class1
						classified: self protocols]]').
	self assert: (class parseTreeForSelector: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeForSelector: #class:) equals: (self parseMethod: 'class: anObject class := anObject').
	self assert: (class classSide parseTreeForSelector: #recursiveSelfRule:) equals: (self parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (class classSide parseTreeForSelector: #recursiveSelfRule) equals: (self parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeForSelector: #builder) equals: (self parseMethod: 'builder ^builder').
	self assert: (class parseTreeForSelector: #builder:) equals: (self parseMethod: 'builder: anObject builder := anObject').
	self assert: (class parseTreeForSelector: #rewriteRule) equals: (self parseMethod: 'rewriteRule ^rewriteRule').
	self assert: (class parseTreeForSelector: #rewriteRule:) equals: (self parseMethod: 'rewriteRule: anObject rewriteRule := anObject')
]

{ #category : 'tests' }
RBMoveMethodParametrizedTest >> testMoveMethodIntoClass [
	| refactoring class |
	(model classFor: Object) compile: 'aTestMethodName ^ OrderedCollection new addAll: (1 to: 100); yourself' classified: #test.

	refactoring := self createRefactoringWithArguments:
				{ #aTestMethodName . model classFor: Object . 'OrderedCollection'}.

	self
		setupMethodNameFor: refactoring
		toReturn: #oneToOneHundred
		withArguments: #().
	refactoring setOption: #variableTypes toUse: [ :ref :initialTypes :selectedTypes | selectedTypes ].
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #Object.
	self
		assert: ((model classFor: Object) parseTreeForSelector: #aTestMethodName)
		equals: (self parseMethod: 'aTestMethodName ^ OrderedCollection oneToOneHundred ').
	self
		assert: ((refactoring model classNamed: #OrderedCollection) classSide parseTreeForSelector: #oneToOneHundred)
		equals: (self parseMethod: 'oneToOneHundred ^ self new addAll: (1 to: 100); yourself')
]

{ #category : 'tests' }
RBMoveMethodParametrizedTest >> testMoveMethodIntoClassVariable [
	| refactoring class |
	self proceedThroughWarning:
		[ refactoring := self createRefactoringWithArguments:
				{ #checkMethod: . RBTransformationRuleTestData . 'RecursiveSelfRule'}.
		self
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: #OCParseTreeSearcher)).
		self
			setupMethodNameFor: refactoring
			toReturn: #foo:foo:
			withArguments: #('transformationRule' 'aSmalllintContext' ).
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class parseTreeForSelector: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext RecursiveSelfRule foo: self foo: aSmalllintContext').
	self assert: ((refactoring model classNamed: #OCParseTreeSearcher) parseTreeForSelector: #foo:foo:) equals: (self parseMethod: 'foo: transformationRule foo: aSmalllintContext
	transformationRule class: aSmalllintContext selectedClass.
	(transformationRule rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
			[(self executeTree: transformationRule rewriteRule tree initialAnswer: false)
				ifFalse:
					[transformationRule builder compile: transformationRule rewriteRule tree printString
						in: transformationRule class1
						classified: aSmalllintContext protocols]]').
	self assert: (class parseTreeForSelector: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeForSelector: #class:) equals: (self parseMethod: 'class: anObject class := anObject').
	self assert: (class parseTreeForSelector: #builder) equals: (self parseMethod: 'builder ^builder').
	self assert: (class parseTreeForSelector: #builder:) equals: (self parseMethod: 'builder: anObject builder := anObject').
	self assert: (class parseTreeForSelector: #rewriteRule) equals: (self parseMethod: 'rewriteRule ^rewriteRule').
	self assert: (class parseTreeForSelector: #rewriteRule:) equals: (self parseMethod: 'rewriteRule: anObject rewriteRule := anObject')
]

{ #category : 'tests' }
RBMoveMethodParametrizedTest >> testMoveMethodIntoInstanceVariable [
	| refactoring class |
	self proceedThroughWarning:
		[ refactoring := self createRefactoringWithArguments:
			  { #checkMethod: . RBTransformationRuleTestData . 'rewriteRule' }.
		self
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: #OCParseTreeRewriter)).
		self
			setupMethodNameFor: refactoring
			toReturn: #foo:foo:
			withArguments: #('transformationRule' 'aSmalllintContext' ).
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class parseTreeForSelector: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext rewriteRule foo: self foo: aSmalllintContext').
	self assert: ((refactoring model classNamed: #OCParseTreeRewriter) parseTreeForSelector: #foo:foo:) equals: (self parseMethod: 'foo: transformationRule foo: aSmalllintContext
	transformationRule class: aSmalllintContext selectedClass.
	(self executeTree: aSmalllintContext parseTree) ifTrue:
			[(transformationRule class recursiveSelfRule executeTree: self tree initialAnswer: false)
				ifFalse:
					[transformationRule builder compile: self tree printString
						in: transformationRule class1
						classified: aSmalllintContext protocols]]').
	self assert: (class parseTreeForSelector: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeForSelector: #class:) equals: (self parseMethod: 'class: anObject class := anObject').
	self assert: (class classSide parseTreeForSelector: #recursiveSelfRule:) equals: (self parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (class classSide parseTreeForSelector: #recursiveSelfRule) equals: (self parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeForSelector: #builder) equals: (self parseMethod: 'builder ^builder').
	self assert: (class parseTreeForSelector: #builder:) equals: (self parseMethod: 'builder: anObject builder := anObject')
]

{ #category : 'tests' }
RBMoveMethodParametrizedTest >> testMoveMethodThatReferencesPoolDictionary [
	| refactoring class |
	self proceedThroughWarning:
		[ refactoring := self createRefactoringWithArguments:
			  { #junk . RBLintRuleTestData . RBClassDataForRefactoringTest name }.
		self
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: RBClassDataForRefactoringTest class name asSymbol)).
		self
			setupMethodNameFor: refactoring
			toReturn: #junk1.
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeForSelector: #junk) equals: (self parseMethod: 'junk ^RBClassDataForRefactoringTest junk1').
	self assert: ((refactoring model metaclassNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #junk1) equals: (self parseMethod: 'junk1
	^self printString copyFrom: 1 to: CR').
	self assert: (class directlyDefinesPoolDictionary: 'TextConstants' asSymbol)
]
