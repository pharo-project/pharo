Class {
	#name : #RBRemoveDirectAccessToVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBRemoveDirectAccessToVariableTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testClassVariable [

	| refactoring class |
	refactoring := (RBRemoveDirectAccessToVariableTransformation
						classVariable: 'UndoSize'
						class: #RBRefactoryChangeManager)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 2.
	
	class := (refactoring model classNamed: #RBRefactoryChangeManager) theMetaClass.
	self assert: (class parseTreeFor: #initialize) 
			equals: (RBParser parseMethod: 'initialize self nuke. self undoSize: 20').
	self assert: (class theNonMetaClass parseTreeFor: #addUndo:) 
			equals: (RBParser parseMethod: 'addUndo: aRefactoringChange
				undo addLast: aRefactoringChange.
				undo size > self class undoSize
					ifTrue: [ undo removeFirst ].
				redo := OrderedCollection new')
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testDoesNotDefineVariable [

	self
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						model: model
						instanceVariable: 'instVarName1'
						class: #Bar)
						asRefactoring;
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						classVariable: 'Foo1'
						class: #RBFooLintRuleTest)
						asRefactoring 
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testDoesNotUnderstandAccessors [

	self
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						model: model
						instanceVariable: 'instVarName1'
						class: #Foo)
						asRefactoring;
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						classVariable: 'RecursiveSelfRule'
						class: #RBTransformationRuleTest)
						asRefactoring 
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testInheritedName [

	self
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						instanceVariable: 'name'
						class: #RBBasicLintRuleTest)
						asRefactoring;
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						classVariable: #DependentsFields
						class: #RBBasicLintRuleTest)
						asRefactoring 
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testInstanceVariable [

	| class |
	(RBRemoveDirectAccessToVariableTransformation 
		model: model
		instanceVariable: 'instVarName2'
		class: #Foo)
		asRefactoring transform.
	
	class := model classNamed: #Foo.
	self assert: (class parseTreeFor: #foo) 
			equals: (RBParser parseMethod: 'foo ^ self instVarName2: 3')
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testMetaclassFailure [

	self shouldFail: (RBRemoveDirectAccessToVariableTransformation 
							classVariable: #RecursiveSelfRule
							class: RBTransformationRuleTest class)
							asRefactoring 
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testNonExistantName [

	self
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						instanceVariable: 'foo'
						class: #RBBasicLintRuleTest)
						asRefactoring;
		shouldFail: (RBRemoveDirectAccessToVariableTransformation 
						classVariable: #Foo
						class: #RBBasicLintRuleTest)
						asRefactoring 
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemoveDirectAccessToVariableTransformation 
						instanceVariable: 'environment'
						class: #RBNamespace)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 6.

	class := refactoring model classNamed: #RBNamespace.
	self assert: (class parseTreeFor: #includesGlobal:)
		equals: (RBParser parseMethod: 'includesGlobal: aSymbol 
			(self hasRemoved: aSymbol) ifTrue: [^false].
			(self includesClassNamed: aSymbol) ifTrue: [^true].
			self environment at: aSymbol ifAbsent: [^false].
			^ true').
	self assert: (class parseTreeFor: #initialize) 
		equals: (RBParser parseMethod: 'initialize
			changes := RBRefactoryChangeManager changeFactory compositeRefactoryChange.
			self environment: RBBrowserEnvironment new.
			newClasses := IdentityDictionary new.
			changedClasses := IdentityDictionary new.
			removedClasses := Set new.
			implementorsCache := IdentityDictionary new.
			sendersCache := IdentityDictionary new')
]

{ #category : #testing }
RBRemoveDirectAccessToVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveDirectAccessToVariableTransformation
							instanceVariable: 'class'
							class: #RBTransformationRuleTest)
							transform.
	
	class := transformation model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #superSends)
		  equals: (RBParser parseMethod: 
	'superSends
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
	
	self assert: (class parseTreeFor: #checkMethod:)
		  equals: (RBParser parseMethod:
	'checkMethod: aSmalllintContext 
		self class: aSmalllintContext selectedClass.
		(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) ifFalse: 
				[builder 
					compile: rewriteRule tree printString
					in: self class
					classified: aSmalllintContext protocols]]')
]
