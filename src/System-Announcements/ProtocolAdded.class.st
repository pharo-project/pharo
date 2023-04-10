"
This class is not used, but should be used when we hook into the addition and removal of protocols. Right now, we only get ClassReorganizedAnnouncement
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
