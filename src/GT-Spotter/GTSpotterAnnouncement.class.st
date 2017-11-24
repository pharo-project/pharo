Class {
	#name : #GTSpotterAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'category'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterAnnouncement >> category [
	"Return the category (ie the kind of group of elements). This has nothing to do with class category."
	^ category
]

{ #category : #accessing }
GTSpotterAnnouncement >> category: anObject [
	"Set the category (ie the kind of group of elements). This has nothing to do with class category."
	category := anObject
]
