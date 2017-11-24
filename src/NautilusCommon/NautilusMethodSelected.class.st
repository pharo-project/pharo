"
I announce when a method is selected, and also contain an information about selected package, class and protocol
"
Class {
	#name : #NautilusMethodSelected,
	#superclass : #NautilusSelectionAnnouncement,
	#category : #'NautilusCommon-Announcements'
}

{ #category : #'instance creation' }
NautilusMethodSelected class >> package: aPackage class: aClass protocol: aProtocol method: aMethod [

	^ self new
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedProtocol: aProtocol;
		selectedMethod: aMethod;
		yourself
]
