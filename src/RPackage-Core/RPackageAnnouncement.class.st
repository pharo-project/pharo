"
Common superclass for package related announcements
"
Class {
	#name : #RPackageAnnouncement,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'package'
	],
	#category : 'RPackage-Core'
}

{ #category : #'instance creation' }
RPackageAnnouncement class >> to: aPackage [

	^ self new package: aPackage
]

{ #category : #accessing }
RPackageAnnouncement >> package [

	^ package
]

{ #category : #accessing }
RPackageAnnouncement >> package: anObject [

	package := anObject
]
