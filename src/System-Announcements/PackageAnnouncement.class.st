"
Common superclass for package related announcements
"
Class {
	#name : 'PackageAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'package'
	],
	#category : 'System-Announcements-System-Packages',
	#package : 'System-Announcements',
	#tag : 'System-Packages'
}

{ #category : 'instance creation' }
PackageAnnouncement class >> to: aPackage [

	^ self new package: aPackage
]

{ #category : 'accessing' }
PackageAnnouncement >> package [

	^ package
]

{ #category : 'accessing' }
PackageAnnouncement >> package: anObject [

	package := anObject
]
