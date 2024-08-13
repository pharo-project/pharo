Class {
	#name : 'RBRemoveDirectAccessToVariableTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBRemoveDirectAccessToVariableTransformationTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBRemoveDirectAccessToVariableTransformationTest >> testClassVariable [

	| refactoring class |
	refactoring := RBRemoveDirectAccessToVariableTransformation
		               classVariable: 'UndoSize'
		               class: #RBRefactoryChangeManager.
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 2.

	class := (refactoring model classNamed: #RBRefactoryChangeManager)
		         classSide.
	self
		assert: (class parseTreeForSelector: #initialize)
		equals:
		(self parseMethod: 'initialize self nuke. self undoSize: 20').
	self
		assert: (class instanceSide parseTreeForSelector: #addUndo:)
		equals: (self parseMethod: 'addUndo: aRefactoringChange
				undo push: aRefactoringChange.
				undo size > self class undoSize
					ifTrue: [ undo removeLast ].
				redo := OrderedCollection new')
]

{ #category : 'tests' }
RBRemoveDirectAccessToVariableTransformationTest >> testDoesNotDefineVariable [

	self
		shouldFail: (RBRemoveDirectAccessToVariableTransformation
				 model: model
				 instanceVariable: 'instVarName1'
				 class: #Bar);
		shouldFail: (RBRemoveDirectAccessToVariableTransformation
				 classVariable: 'Foo1'
				 class: #RBFooLintRuleTestData)
]

{ #category : 'tests' }
RBRemoveDirectAccessToVariableTransformationTest >> testDoesNotUnderstandAccessors [

	self
		shouldFail: (RBRemoveDirectAccessToVariableTransformation
				 model: model
				 instanceVariable: 'instVarName1'
				 class: #Foo);
		shouldFail: (RBRemoveDirectAccessToVariableTransformation
				 classVariable: 'RecursiveSelfRule'
				 class: #RBTransformationRuleTestData)
]

{ #category : 'tests' }
RBRemoveDirectAccessToVariableTransformationTest >> testInstanceVariable [

	| class |
	(RBRemoveDirectAccessToVariableTransformation
		 model: model
		 instanceVariable: 'instVarName2'
		 class: #Foo) generateChanges.

	class := model classNamed: #Foo.
	self
		assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo ^ self instVarName2: 3')
]

{ #category : 'tests' }
RBRemoveDirectAccessToVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := RBRemoveDirectAccessToVariableTransformation
		               instanceVariable: 'environment'
		               class: #RBNamespace.
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 4.

	class := refactoring model classNamed: #RBNamespace.
	self
		assert: (class parseTreeForSelector: #includesGlobal:)
		equals: (self parseMethod: 'includesGlobal: aSymbol
			(self hasRemoved: aSymbol) ifTrue: [^false].
			(self includesClassNamed: aSymbol) ifTrue: [^true].
			self environment at: aSymbol ifAbsent: [^false].
			^ true').
	self
		assert: (class parseTreeForSelector: #initialize)
		equals: (self parseMethod: 'initialize
	super initialize.
	changes := changeFactory compositeRefactoryChange onSystemEnvironment: self environment.
	newClasses := IdentityDictionary new.
	newPackages := IdentityDictionary new.
	changedClasses := IdentityDictionary new.
	changedPackages := IdentityDictionary new.
	removedClasses := Set new.
	removedPackages := Set new.
	implementorsCache := IdentityDictionary new.
	sendersCache := IdentityDictionary new')
]

{ #category : 'tests' }
RBRemoveDirectAccessToVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := RBRemoveDirectAccessToVariableTransformation
		                  instanceVariable: 'class'
		                  class: #RBTransformationRuleTestData.
	transformation generateChanges.

	class := transformation model classNamed:
		         #RBTransformationRuleTestData.
	self
		assert: (class parseTreeForSelector: #superSends)
		equals: (self parseMethod: 'superSends
		| rule |
		rule := RBParseTreeRewriter new.
		rule addSearch: ''super `@message: ``@args''
			-> (
				[:aNode |
				(self class withAllSubclasses
					detect: [:each | each includesSelector: aNode selector]
					ifNone: [nil]) isNil]
						-> ''self `@message: ``@args'').
		self rewriteUsing: rule').

	self
		assert: (class parseTreeForSelector: #checkMethod:)
		equals: (self parseMethod: 'checkMethod: aSmalllintContext
		self class: aSmalllintContext selectedClass.
		(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) ifFalse:
				[builder
					compile: rewriteRule tree printString
					in: self class
					classified: aSmalllintContext protocols]]')
]
