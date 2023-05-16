"
I am an announcement raised when we rename a protocol.

I should not be used because I will soon be removed since a rename is an addition and a removal in the end.
"
Class {
	#name : #ProtocolRenamed,
	#superclass : #ProtocolAnnouncement,
	#instVars : [
		'newProtocol'
	],
	#category : #'System-Announcements-System-Protocols'
}

{ #category : #'class initialization' }
ProtocolRenamed class >> in: aClass from: oldName to: newName [

	^ (self in: aClass protocol: oldName)
		  newProtocol: newName;
		  yourself
]

{ #category : #testing }
ProtocolRenamed class >> isDeprecated [
	"Renaming is a composition of ProtocolAdded and ProtocolRemoved. To act on a renaming, those two are the announcements to listen to."

	^ true
]

{ #category : #accessing }
ProtocolRenamed >> newProtocol [

	^ newProtocol
]

{ #category : #accessing }
ProtocolRenamed >> newProtocol: anObject [

	newProtocol := anObject
]

{ #category : #accessing }
ProtocolRenamed >> oldProtocol [

	^ self protocol
]

{ #category : #accessing }
ProtocolRenamed >> oldProtocol: aString [

	self protocol: aString
]
