"
I'm a generic announcement to notify when the repository registry has changed.
"
Class {
	#name : #IceRepositoryRegistryAnnouncement,
	#superclass : #IceRepositoryAnnouncement,
	#category : #'Iceberg-Announcements'
}

{ #category : #'instance creation' }
IceRepositoryRegistryAnnouncement class >> repository: aRepository [
	^ self new repository: aRepository; yourself
]

{ #category : #accessing }
IceRepositoryRegistryAnnouncement >> repository [
	^ repository
]

{ #category : #accessing }
IceRepositoryRegistryAnnouncement >> repository: anObject [
	repository := anObject
]
