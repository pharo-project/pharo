"
RBRefactoryProtocolChange is responsible for removing a protocol in classes in a RB model
"
Class {
	#name : #RBRemoveProtocolChange,
	#superclass : #RBRefactoryProtocolChange,
	#category : #'Refactoring2-Core-Changes'
}

{ #category : #'instance creation' }
RBRemoveProtocolChange class >> removeProtocolNamed: aString in: aClass [

	^ self new
		changeClass: aClass;
		protocol: aString;
		yourself
]

{ #category : #converting }
RBRemoveProtocolChange >> asUndoOperation [
	
	^ RBAddProtocolChange addProtocolNamed: protocol in: self changeClass 
]

{ #category : #printing }
RBRemoveProtocolChange >> changeString [

	^ 'Remove protocol named ', self displayProtocolName, ' in ', self displayClassName
]

{ #category : #private }
RBRemoveProtocolChange >> primitiveExecute [

	self changeClass organization removeProtocol: ( self changeClass organization protocolNamed: protocol )
]

{ #category : #printing }
RBRemoveProtocolChange >> printOn: aStream [ 

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeProtocolNamed: ';
		print: ( self displayProtocolName );
		nextPutAll: ' stamp: ';
		print: (self changeStamp);
		nextPutAll: '!'
]
