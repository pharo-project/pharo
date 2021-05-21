"
I am responsible for changing the protocol (or category) of a method. If the protocol does not exist in the class protocols list, the transformation will create it automatically.

Usage: 
| transformation |
transformation := (RBMethodProtocolTransformation
				protocol: 'transforming'
				inMethod: #someMethod
				inClass: #RBDummyEmptyClass)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the method exists.
"
Class {
	#name : #RBMethodProtocolTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'protocol'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBMethodProtocolTransformation class >> model: aRBModel protocol: aString inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		protocol: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBMethodProtocolTransformation class >> protocol: aString inMethod: aSelector inClass: aClassName [

	^ self new
		protocol: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #preconditions }
RBMethodProtocolTransformation >> preconditions [

	^ (RBCondition
			withBlock: [ self definingClass isNotNil ]
			errorString: ('Class named <1s> does not exist' expandMacrosWith: className))
		& (RBCondition definesSelector: selector in: self definingClass)
]

{ #category : #executing }
RBMethodProtocolTransformation >> privateTransform [

	self model
		selector: self definingMethod selector
		in: self definingClass
		classified: self protocol.
]

{ #category : #executing }
RBMethodProtocolTransformation >> protocol [

	^ protocol ifNil: [ 
		self uiManager
			chooseOrRequestFrom: self definingClass protocols
			title: 'Class name was not defined to perform ', self class baseClass name,
					 '. Please select a class to resume this transformation.'  ]
]

{ #category : #api }
RBMethodProtocolTransformation >> protocol: aString inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	protocol := aString
]

{ #category : #printing }
RBMethodProtocolTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' protocol ''';
		nextPutAll: protocol;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
