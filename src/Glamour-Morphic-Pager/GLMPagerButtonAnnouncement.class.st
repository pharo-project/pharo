Class {
	#name : #GLMPagerButtonAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'buttonModel'
	],
	#category : #Glamour-Morphic-Pager-Announcement
}

{ #category : #accessing }
GLMPagerButtonAnnouncement >> buttonModel [
	^ buttonModel
]

{ #category : #accessing }
GLMPagerButtonAnnouncement >> buttonModel: anObject [
	buttonModel := anObject
]
