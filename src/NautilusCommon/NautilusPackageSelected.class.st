"
I announce when a package is selected
"
Class {
	#name : #NautilusPackageSelected,
	#superclass : #NautilusSelectionAnnouncement,
	#category : #NautilusCommon-Announcements
}

{ #category : #'instance creation' }
NautilusPackageSelected class >> package: aPackage [

	^ self new
		selectedPackage: aPackage;
		yourself
]
