"
Announcement sent when a Repository is removed from  a Repository group.
"
Class {
	#name : 'MCRepositoryRemoved',
	#superclass : 'Announcement',
	#instVars : [
		'repository'
	],
	#category : 'Monticello-Announcements',
	#package : 'Monticello',
	#tag : 'Announcements'
}

{ #category : 'accessing' }
MCRepositoryRemoved >> repository [
	^ repository
]

{ #category : 'accessing' }
MCRepositoryRemoved >> repository: anObject [
	repository := anObject
]
