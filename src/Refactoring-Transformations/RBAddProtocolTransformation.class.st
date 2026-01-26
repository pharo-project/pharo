"
I am responsible to add a protocol to a class.

Usage:
```
| transformation |
transformation := (RBAddProtocolTransformation
				protocol: 'transforming'
				inClass: #RBDummyEmptyClass)
				generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- The class exists;
- The protocol name is a valid name.
"
Class {
	#name : 'RBAddProtocolTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'protocol'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBAddProtocolTransformation class >> model: aRBModel protocol: aString inClass: aClassName [

	^ self new
		model: aRBModel;
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : 'api' }
RBAddProtocolTransformation class >> protocol: aString inClass: aClassName [

	^ self new
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : 'preconditions' }
RBAddProtocolTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  self protocolExist }
]

{ #category : 'scripting api - conditions' }
RBAddProtocolTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBAddProtocolTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString: 'Class named ' , className , ' does not exist'
]

{ #category : 'executing' }
RBAddProtocolTransformation >> privateTransform [

	self definingClass addProtocolNamed: protocol
]

{ #category : 'api' }
RBAddProtocolTransformation >> protocol: aString inClass: aClassName [

	self className: aClassName.
	protocol := aString
]

{ #category : 'preconditions' }
RBAddProtocolTransformation >> protocolExist [

	^ RBCondition
		  withBlock: [
			  ((model environment protocolsFor: self definingClass realClass)
				   includes: protocol) not ]
		  errorString: 'Protocol named ' , protocol , ' already exists'
]

{ #category : 'storing' }
RBAddProtocolTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' protocol: ''';
		nextPutAll: protocol;
		nextPutAll: ''' inClass: ''';
		nextPutAll: className.
	aStream nextPut: $)
]
