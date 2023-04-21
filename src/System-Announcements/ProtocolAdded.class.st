"
I am an announcement raised when we add a new protocol in a class.

I know the protocol that was added and allows users to react to it.
"
Class {
	#name : #ProtocolAdded,
	#superclass : #ProtocolAnnouncement,
	#category : #'System-Announcements-System-Protocols'
}

{ #category : #'class initialization' }
ProtocolAdded class >> in: aClass protocol: aProtocolName [

	^ self new
		  classReorganized: aClass;
		  protocol: aProtocolName;
		  yourself
]
