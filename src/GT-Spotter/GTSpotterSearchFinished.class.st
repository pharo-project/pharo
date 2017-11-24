Class {
	#name : #GTSpotterSearchFinished,
	#superclass : #Announcement,
	#instVars : [
		'processors',
		'query'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterSearchFinished >> processors [
	^ processors
]

{ #category : #accessing }
GTSpotterSearchFinished >> processors: anObject [
	processors := anObject
]

{ #category : #accessing }
GTSpotterSearchFinished >> query [
	^ query
]

{ #category : #accessing }
GTSpotterSearchFinished >> query: anObject [
	query := anObject
]
