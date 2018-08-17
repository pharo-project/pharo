"
I am responsible to add a protocol to a class.

Usage:
| transformation |
transformation := (RBAddProtocolTransformation
				protocol: 'transforming'
				inClass: #RBDummyEmptyClass)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- The class exists;
- The protocol name is a valid name.
"
Class {
	#name : #RBAddProtocolTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'protocol'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddProtocolTransformation class >> model: aRBModel protocol: aString inClass: aClassName [

	^ self new
		model: aRBModel;
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddProtocolTransformation class >> protocol: aString inClass: aClassName [

	^ self new
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : #preconditions }
RBAddProtocolTransformation >> preconditions [

	^ ( RBCondition
			withBlock: [ self definingClass isNotNil ]
			errorString: 'Class named ', className, ' does not exist' )
	  & ( RBCondition withBlock: [ (( model environment 
			protocolsFor: self definingClass realClass )
			includes: protocol ) not ]
			errorString: 'Protocol named ', protocol, ' already exists' )
]

{ #category : #executing }
RBAddProtocolTransformation >> privateTransform [

	self definingClass addProtocolNamed: protocol
]

{ #category : #api }
RBAddProtocolTransformation >> protocol: aString inClass: aClassName [

	self className: aClassName.
	protocol := aString
]

{ #category : #printing }
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
