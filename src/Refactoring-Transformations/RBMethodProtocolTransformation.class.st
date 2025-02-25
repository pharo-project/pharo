"
I am responsible for changing the protocol (or category) of a method. If the protocol does not exist in the class protocols list, the transformation will create it automatically.

Usage:
```
| transformation |
transformation := (RBMethodProtocolTransformation
				protocol: 'transforming'
				inMethod: #someMethod
				inClass: #RBDummyEmptyClass)
		generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the method exists.
"
Class {
	#name : 'RBMethodProtocolTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'protocol'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'testing' }
RBMethodProtocolTransformation class >> isTransformation [

	^ true
]

{ #category : 'api' }
RBMethodProtocolTransformation class >> model: aRBModel protocol: aString inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		protocol: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'api' }
RBMethodProtocolTransformation class >> protocol: aString inMethod: aSelector inClass: aClassName [

	^ self new
		protocol: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'preconditions' }
RBMethodProtocolTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  (RBCondition definesSelector: selector in: self definingClass).
		  (RBCondition
			   withBlock: [ protocol isNotNil ]
			   errorString:
			   ('Protocol of <1s> should not be nil' expandMacrosWith:
				    className)) }
]

{ #category : 'scripting api - conditions' }
RBMethodProtocolTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBMethodProtocolTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString:
		  ('Class named <1s> does not exist' expandMacrosWith: className)
]

{ #category : 'executing' }
RBMethodProtocolTransformation >> privateTransform [

	self model
		selector: self definingMethod selector
		in: self definingClass
		classified: protocol
]

{ #category : 'api' }
RBMethodProtocolTransformation >> protocol: aString inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	protocol := aString
]

{ #category : 'storing' }
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
