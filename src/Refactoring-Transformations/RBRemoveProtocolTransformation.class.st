"
I am responsible for removing a protocol in a class

Usage:
```
| transformation |
transformation := (RBRemoveProtocolTransformation
			protocol: 'empty protocol 1'
			inClass: #RBDummyEmptyClass)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```
	
Preconditions:
- The class exists;
- A protocol with this name exists;
- No methods are categorized with this protocol.
"
Class {
	#name : 'RBRemoveProtocolTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'protocol'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveProtocolTransformation class >> model: aRBModel protocol: aString inClass: aClassName [

	^ self new
		model: aRBModel;
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : 'api' }
RBRemoveProtocolTransformation class >> protocol: aString inClass: aClassName [

	^ self new
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : 'preconditions' }
RBRemoveProtocolTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  (RBCondition
			   withBlock: [
				   (model environment protocolsFor: self definingClass realClass)
					   anySatisfy: [ :prot | prot name = protocol ] ]
			   errorString: 'Protocol named ' , protocol , ' does not exist').
		  (RBCondition
			   withBlock: [
			   (self definingClass realClass protocolNamed: protocol) isEmpty ]
			   errorString: 'Protocol named ' , protocol
				   , ' is not empty and it cannot be removed') }
]

{ #category : 'scripting api - conditions' }
RBRemoveProtocolTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBRemoveProtocolTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString: 'Class named ' , className , ' does not exist' 
]

{ #category : 'executing' }
RBRemoveProtocolTransformation >> privateTransform [

	self definingClass removeProtocolNamed: protocol
]

{ #category : 'api' }
RBRemoveProtocolTransformation >> protocol: aString inClass: aClassName [

	self className: aClassName.
	protocol := aString
]

{ #category : 'storing' }
RBRemoveProtocolTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' protocol: ''';
		nextPutAll: protocol;
		nextPutAll: ''' inClass: ''';
		nextPutAll: className.
	aStream nextPut: $)
]
