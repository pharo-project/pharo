"
I announce when a package has been loaded.
"
Class {
	#name : #IcePackageLoaded,
	#superclass : #IceAnnouncement,
	#instVars : [
		'loadedVersion'
	],
	#category : #'Iceberg-Announcements'
}

{ #category : #'instance creation' }
IcePackageLoaded class >> version: version [ 
	^ self new loadedVersion: version
]

{ #category : #accessing }
IcePackageLoaded >> loadedVersion [
	^ loadedVersion
]

{ #category : #accessing }
IcePackageLoaded >> loadedVersion: anObject [
	loadedVersion := anObject
]
