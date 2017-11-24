"
This is event about protocol rename. It should substitute ClassReorganized announcement 
"
Class {
	#name : #ProtocolRenamed,
	#superclass : #ProtocolAnnouncement,
	#instVars : [
		'newProtocolName'
	],
	#category : #'System-Announcements-System-Protocols'
}

{ #category : #accessing }
ProtocolRenamed >> newProtocolName [
	^ newProtocolName
]

{ #category : #accessing }
ProtocolRenamed >> newProtocolName: anObject [
	newProtocolName := anObject
]

{ #category : #accessing }
ProtocolRenamed >> oldProtocolName [
	^ protocol
]

{ #category : #accessing }
ProtocolRenamed >> oldProtocolName: aString [
	self protocol: aString
]
