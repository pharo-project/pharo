"
I announce when a diff has been refreshed.
"
Class {
	#name : #IceTipDiffRefreshed,
	#superclass : #IceTipAnnouncement,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-TipUI-Announcements'
}

{ #category : #'instance creation' }
IceTipDiffRefreshed class >> for: aRepository [
	^ self new repository: aRepository
]

{ #category : #accessing }
IceTipDiffRefreshed >> repository [
	^ repository
]

{ #category : #accessing }
IceTipDiffRefreshed >> repository: anObject [
	repository := anObject
]
