"
RBRefactoryProtocolChange is responsible for adding a protocol in classes in a RB model
"
Class {
	#name : 'ReAddProtocolChange',
	#superclass : 'ReProtocolChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReAddProtocolChange class >> addProtocolNamed: aString in: aClass [

	^ self new
		changeClass: aClass;
		protocol: aString;
		yourself
]

{ #category : 'converting' }
ReAddProtocolChange >> asUndoOperation [

	^ ReRemoveProtocolChange removeProtocolNamed: protocol in: self changeClass
]

{ #category : 'printing' }
ReAddProtocolChange >> changeString [

	^ 'Add Protocol named ', self displayProtocolName, ' in ', self displayClassName
]

{ #category : 'private' }
ReAddProtocolChange >> generateChanges [

	self changeClass addProtocol: protocol
]

{ #category : 'printing' }
ReAddProtocolChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' addProtocolNamed: ';
		print: self displayProtocolName;
		nextPutAll: ' stamp: ';
		print: self changeStamp
]
