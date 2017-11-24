Class {
	#name : #GTSpotterDiveInElement,
	#superclass : #Announcement,
	#instVars : [
		'element',
		'eventType'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterDiveInElement >> element [
	^ element
]

{ #category : #accessing }
GTSpotterDiveInElement >> element: anObject [
	element := anObject
]

{ #category : #accessing }
GTSpotterDiveInElement >> eventType [
	^ eventType
]

{ #category : #accessing }
GTSpotterDiveInElement >> eventType: anObject [
	eventType := anObject
]
