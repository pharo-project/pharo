"
Announcement sent when a Repository is added to a Repository group.
"
Class {
	#name : #MCRepositoryAdded,
	#superclass : #Announcement,
	#instVars : [
		'repository'
	],
	#category : 'Monticello-Announcements'
}

{ #category : #accessing }
MCRepositoryAdded >> repository [
	^ repository
]

{ #category : #accessing }
MCRepositoryAdded >> repository: anObject [
	repository := anObject
]
