"
I announce when a protocol is selected, and also contain an information about selected package and class
"
Class {
	#name : #NautilusProtocolSelected,
	#superclass : #NautilusSelectionAnnouncement,
	#category : 'NautilusCommon-Announcements'
}

{ #category : #'instance creation' }
NautilusProtocolSelected class >> package: aPackage class: aClass protocol: aProtocol [

	^ self new
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedProtocol: aProtocol;
		yourself
]
