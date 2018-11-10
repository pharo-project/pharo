"
I'm a generic announcement to notify when the repository registry has changed.
"
Class {
	#name : #IceRepositoryRegistryModified,
	#superclass : #Announcement,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Announcements'
}

{ #category : #'instance creation' }
IceRepositoryRegistryModified class >> repository: aRepository [
	^ self new repository: aRepository; yourself
]

{ #category : #accessing }
IceRepositoryRegistryModified >> repository [
	^ repository
]

{ #category : #accessing }
IceRepositoryRegistryModified >> repository: anObject [
	repository := anObject
]
