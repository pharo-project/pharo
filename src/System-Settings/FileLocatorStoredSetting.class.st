"
I am responsible for storing information about FileLocator objects.
"
Class {
	#name : #FileLocatorStoredSetting,
	#superclass : #AbstractStoredSetting,
	#instVars : [
		'origin',
		'pathSegments'
	],
	#category : #System-Settings-Persistence-Ston-StoredSettings
}

{ #category : #accessing }
FileLocatorStoredSetting >> origin: anObject [
	origin := anObject
]

{ #category : #accessing }
FileLocatorStoredSetting >> pathSegments: anObject [
	pathSegments := anObject
]

{ #category : #accessing }
FileLocatorStoredSetting >> realValue [
	^ FileLocator origin: origin path: (RelativePath withAll: pathSegments)
]
