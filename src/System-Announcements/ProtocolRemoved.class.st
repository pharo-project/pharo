"
This class is not used, but should be used when we hook into the addition and removal of protocols. Right now, we only get ClassReorganizedAnnouncement
"
Class {
	#name : #ProtocolRemoved,
	#superclass : #ProtocolAnnouncement,
	#category : #'System-Announcements-System-Protocols'
}

{ #category : #'class initialization' }
ProtocolRemoved class >> in: aClass protocol: aProtocolName [

	^ self new
		  classReorganized: aClass;
		  protocol: aProtocolName;
		  yourself
]
