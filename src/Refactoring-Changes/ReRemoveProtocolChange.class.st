"
RBRefactoryProtocolChange is responsible for removing a protocol in classes in a RB model
"
Class {
	#name : 'ReRemoveProtocolChange',
	#superclass : 'ReProtocolChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReRemoveProtocolChange class >> removeProtocolNamed: aString in: aClass [

	^ self new
		changeClass: aClass;
		protocol: aString;
		yourself
]

{ #category : 'converting' }
ReRemoveProtocolChange >> asUndoOperation [

	^ ReAddProtocolChange addProtocolNamed: protocol in: self changeClass
]

{ #category : 'printing' }
ReRemoveProtocolChange >> changeString [

	^ 'Remove protocol named ', self displayProtocolName, ' in ', self displayClassName
]

{ #category : 'private' }
ReRemoveProtocolChange >> generateChanges [

	self changeClass removeProtocolIfEmpty: protocol
]

{ #category : 'printing' }
ReRemoveProtocolChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeProtocolNamed: ';
		print: self displayProtocolName;
		nextPutAll: ' stamp: ';
		print: self changeStamp
]
