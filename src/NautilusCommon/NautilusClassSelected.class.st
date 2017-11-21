"
I announce when a class is selected, and also contain an information about selected package
"
Class {
	#name : #NautilusClassSelected,
	#superclass : #NautilusSelectionAnnouncement,
	#category : #NautilusCommon-Announcements
}

{ #category : #'instance creation' }
NautilusClassSelected class >> package: aPackage class: aClass [

	^ self new
		selectedPackage: aPackage;
		selectedClass: aClass;
		yourself
]
