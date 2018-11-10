"
Groupes different announcements that can happen in a repository.
"
Class {
	#name : #IceRepositoryAnnouncement,
	#superclass : #IceAnnouncement,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Announcements'
}

{ #category : #'instance creation' }
IceRepositoryAnnouncement class >> for: repository [
	^ self new repository: repository; yourself
]

{ #category : #testing }
IceRepositoryAnnouncement >> appliesToRepository: aRepository [
	^ self repository = aRepository
]

{ #category : #accessing }
IceRepositoryAnnouncement >> repository [
	^ repository
]

{ #category : #accessing }
IceRepositoryAnnouncement >> repository: anObject [
	repository := anObject
]
