"
An MCPackageModified is raised when a MCPackage is modified
"
Class {
	#name : #MCPackageModified,
	#superclass : #Announcement,
	#instVars : [
		'package'
	],
	#category : #Monticello-Announcements
}

{ #category : #'instance creation' }
MCPackageModified class >> package: aMCPackage [

	^ self new package: aMCPackage
]

{ #category : #accessing }
MCPackageModified >> package [

		^ package
]

{ #category : #accessing }
MCPackageModified >> package: anObject [
	
		package := anObject
]
