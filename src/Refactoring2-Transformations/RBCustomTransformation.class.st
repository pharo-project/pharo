"
Performs a custom transformation. With this class, the developer can manipulate the model (a RBNamespace) directly, i.e., adding classes, transforming them etc.

Usage: 
transformation := (RBCustomTransformation with: [ :model |
	model defineClass: ('Object subclass: #Bla
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ',  'Refactoring2-Transformations' surroundedBySingleQuotes ).
	(model classNamed: #Bla)
		comment: 'Deprecated!!! Use super class' ])
	transform.
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : #RBCustomTransformation,
	#superclass : #RBTransformation,
	#instVars : [
		'transformationBlock'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBCustomTransformation class >> model: aRBModel with: aBlockWithOneArg [

	^ self new
		model: aRBModel;
		transformationBlock: aBlockWithOneArg;
		yourself
]

{ #category : #api }
RBCustomTransformation class >> with: aBlockWithOneArg [

	^ self new
		transformationBlock: aBlockWithOneArg;
		yourself
]

{ #category : #preconditions }
RBCustomTransformation >> preconditions [
	"no precondition so far"
	
	^ RBCondition empty
]

{ #category : #executing }
RBCustomTransformation >> privateTransform [

	transformationBlock value: self model
]

{ #category : #printing }
RBCustomTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' with: '.
	transformationBlock storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #api }
RBCustomTransformation >> transformationBlock: aBlockWithOneArg [

	transformationBlock := aBlockWithOneArg
]
