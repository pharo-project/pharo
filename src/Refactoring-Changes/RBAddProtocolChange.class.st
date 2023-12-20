"
RBRefactoryProtocolChange is responsible for adding a protocol in classes in a RB model
"
Class {
	#name : 'RBAddProtocolChange',
	#superclass : 'RBRefactoryProtocolChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
RBAddProtocolChange class >> addProtocolNamed: aString in: aClass [

	^ self new
		changeClass: aClass;
		protocol: aString;
		yourself
]

{ #category : 'converting' }
RBAddProtocolChange >> asUndoOperation [

	^ RBRemoveProtocolChange removeProtocolNamed: protocol in: self changeClass
]

{ #category : 'printing' }
RBAddProtocolChange >> changeString [

	^ 'Add Protocol named ', self displayProtocolName, ' in ', self displayClassName
]

{ #category : 'private' }
RBAddProtocolChange >> generateChanges [

	self changeClass addProtocol: protocol
]

{ #category : 'printing' }
RBAddProtocolChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' addProtocolNamed: ';
		print: self displayProtocolName;
		nextPutAll: ' stamp: ';
		print: self changeStamp
]
