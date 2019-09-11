"
Adds a method in a class. The source code of the method, as well as the protocol in which the method will be categorized, can be specified.

Usage:
| transformation |
transformation := (RBAddMethodTransformation
		sourceCode: 'printString1 ^super printString'
		in: RBTransformationTest
		withProtocol: #accessing)
		transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
The source code of the method must be syntactically valid.
"
Class {
	#name : #RBAddMethodTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'sourceCode',
		'protocol'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddMethodTransformation class >> model: aRBModel sourceCode: aString in: aClass withProtocol: protocol [

	^ self new
		model: aRBModel;
		sourceCode: aString
		in: aClass
		withProtocol: protocol asSymbol;
		yourself
]

{ #category : #api }
RBAddMethodTransformation class >> sourceCode: aString in: aClass withProtocol: protocol [

	^ self new
		sourceCode: aString
		in: aClass
		withProtocol: protocol asSymbol;
		yourself
]

{ #category : #preconditions }
RBAddMethodTransformation >> preconditions [
	| rbMethod |
	rbMethod := self parserClass
		parseMethod: sourceCode
		onError: [ :string :position | 
			^ RBCondition
				withBlock: [ self
						refactoringError: 'The content of this method cannot be parsed.' ] ].
	rbMethod selector
		ifNil: [ self refactoringError: 'The method has no selector.' ].
	^ (RBCondition
		canUnderstand: rbMethod selector
		in: self definingClass) not
]

{ #category : #executing }
RBAddMethodTransformation >> privateTransform [

	self definingClass
		compile: sourceCode
		classified: (Array with: protocol)
]

{ #category : #api }
RBAddMethodTransformation >> sourceCode: aString in: aClassName withProtocol: aSymbol [

	self className: aClassName.
	sourceCode := aString.
	protocol := aSymbol asSymbol
]

{ #category : #printing }
RBAddMethodTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' sourceCode: '''.
	sourceCode storeOn: aStream.
	aStream nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' withProtocol: #';
		nextPutAll: protocol asString.
	aStream nextPut: $)
]
