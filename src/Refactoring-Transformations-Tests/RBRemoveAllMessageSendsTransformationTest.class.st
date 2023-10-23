Class {
	#name : 'RBRemoveAllMessageSendsTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRemoveAllMessageSendsTransformationTest >> testClassDoesNotExist [

	| trans |
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #foo:
				inMethod: #badMessage1
				inClass: #ADoesNotExistClass).
	self should: [ trans generateChanges ] raise: trans refactoringErrorClass.
]

{ #category : 'failure tests' }
RBRemoveAllMessageSendsTransformationTest >> testFailureNonExistantSelector [

	| trans |
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #foo:
				inMethod: #checkClass1:
				inClass: #RBClassDataForRefactoringTest).
	self should: [ trans generateChanges ] raise: trans refactoringErrorClass.
]

{ #category : 'failure tests' }
RBRemoveAllMessageSendsTransformationTest >> testMethodDoesNotExist [
		
	| trans |
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #foo:
				inMethod: #badMessage1
				inClass: #RBClassDataForRefactoringTest).
	self should: [ trans generateChanges ] raise: trans refactoringErrorClass.
]

{ #category : 'tests' }
RBRemoveAllMessageSendsTransformationTest >> testRemoveMessageInsideBlock [

	| trans |
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #printString
				inMethod: #caller1
				inClass: #RBClassDataForRefactoringTest).
	trans generateChanges.

	self assert: ((trans model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #caller1) equals: (self parseMethod: 'caller1
	| anObject |
	anObject := 5.
	self called: anObject + 1
		on1:
			[:each |
			^anObject]')
]

{ #category : 'tests' }
RBRemoveAllMessageSendsTransformationTest >> testRemoveSenderIntoCascaded2Message [

	| transformedMethod afterRefactoring1 afterRefactoring2 trans |
	afterRefactoring1 := self parseMethod: 'inlineMethod
	| temp |
	self foo.
	temp := self bar.
	^ temp'.
	afterRefactoring2 := self parseMethod: 'inlineMethod
	| temp |
	temp := self
		        foo;
		        bar.
	^ temp'.
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #inlineMethod
				inMethod: #inlineMethod
				inClass: #RBClassDataForRefactoringTest).
	trans generateChanges.
	transformedMethod := ((trans model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineMethod).
	self assert: (transformedMethod = afterRefactoring1 or: [ transformedMethod = afterRefactoring2])
]

{ #category : 'tests' }
RBRemoveAllMessageSendsTransformationTest >> testRemoveSenderIntoCascaded3Message [

	| transformedMethod afterRefactoring1 afterRefactoring2 trans |
	afterRefactoring1 := self parseMethod: 'referencesConditionFor: aClass

	| environment association anObject |
	anObject := RBCondition withBlock: [
		            association := Smalltalk globals
			                           associationAt: aClass name
			                           ifAbsent: [
			                           self refactoringError:
				                           ''Could not find class'' ].
		            environment := (self environment referencesTo:
			                            association)
		                           |
		                           (self environment referencesTo:
			                            aClass name).
		            environment isEmpty ].
	anObject errorMacro: aClass , '' is referenced.<n>Browse references?''.
	^ anObject yourself' .
	afterRefactoring2 := self parseMethod: 'referencesConditionFor: aClass
	| environment association |
	^ (RBCondition withBlock: [
			   association := Smalltalk globals
				                  associationAt: aClass name
				                  ifAbsent: [
					                  self refactoringError: ''Could not find class'' ].
			   environment := (self environment referencesTo: association)
			                  | (self environment referencesTo: aClass name).
			   environment isEmpty ])
		  errorMacro: aClass , '' is referenced.<n>Browse references?'';
		  yourself'.
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #errorBlock:
				inMethod: #referencesConditionFor:
				inClass: #RBClassDataForRefactoringTest).
	trans generateChanges.
	transformedMethod := ((trans model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #referencesConditionFor:).
	self assert: (transformedMethod = afterRefactoring1 or: [ transformedMethod = afterRefactoring2])
]

{ #category : 'tests' }
RBRemoveAllMessageSendsTransformationTest >> testRemoveSenderIntoCascadedMessage [

	| trans |
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #cr
				inMethod: (#called:, #on:) asSymbol
				inClass: #RBClassDataForRefactoringTest).
	trans generateChanges.

	self assert: ((trans model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: (#called:, #on:) asSymbol)
		equals: (self parseMethod: 'called: anObject on: aBlock
			anObject printString traceCr.
			aBlock value')
]

{ #category : 'tests' }
RBRemoveAllMessageSendsTransformationTest >> testRemoveSimpleSenderOfMessage [

	|  transformedMethod afterRefactoring1 afterRefactoring2 trans |
	afterRefactoring1 := self parseMethod: 'caller1
	| anObject arg1 |
	anObject := 5.
	arg1 := anObject + 1'.
	afterRefactoring2 :=  self parseMethod: 'caller1
	| anObject |
	anObject := 5'.
	
	trans := (RBRemoveAllMessageSendsTransformation
				messageSend: #called:on1:
				inMethod: #caller1
				inClass: #RBClassDataForRefactoringTest).
	trans generateChanges.
	transformedMethod := ((trans model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #caller1).
	self assert: (transformedMethod = afterRefactoring1 or: [ transformedMethod = afterRefactoring2])
]
