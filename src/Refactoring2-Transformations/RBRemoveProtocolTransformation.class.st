"
I am responsible for removing a protocol in a class

Usage:
| transformation |
transformation := (RBRemoveProtocolTransformation
			protocol: 'empty protocol'
			inClass: #RBDummyEmptyClass)
			transform.
(ChangesBrowser changes: transformation model changes changes) open
	
Preconditions:
- The class exists;
- A protocol with this name exists;
- No methods are categorized with this protocol.
"
Class {
	#name : #RBRemoveProtocolTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'protocol'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRemoveProtocolTransformation class >> model: aRBModel protocol: aString inClass: aClassName [

	^ self new
		model: aRBModel;
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBRemoveProtocolTransformation class >> protocol: aString inClass: aClassName [

	^ self new
		protocol: aString
		inClass: aClassName;
		yourself
]

{ #category : #preconditions }
RBRemoveProtocolTransformation >> preconditions [

	^ ( RBCondition
			withBlock: [ self definingClass isNotNil ]
			errorString: 'Class named ', className, ' does not exist' ) 
		& ( RBCondition
			withBlock: [ ( model environment 
			protocolsFor: self definingClass realClass )
			includes: protocol ]
			errorString: 'Protocol named ', protocol, ' does not exist' )
		& ( RBCondition
			withBlock: [ (self definingClass realClass organization protocolNamed: protocol ) canBeRemoved ]
			errorString: 'Protocol named ', protocol, ' is a virtual protocol and it cannot be removed' ) 
		& ( RBCondition
			withBlock: [ (self definingClass realClass organization protocolNamed: protocol ) isEmpty ]
			errorString: 'Protocol named ', protocol, ' is not empty and it cannot be removed' )
]

{ #category : #executing }
RBRemoveProtocolTransformation >> privateTransform [

	self definingClass removeProtocolNamed: protocol
]

{ #category : #api }
RBRemoveProtocolTransformation >> protocol: aString inClass: aClassName [

	self className: aClassName.
	protocol := aString
]

{ #category : #printing }
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
