"
A SearchEnded is an announcement raised when a search has ended.
"
Class {
	#name : #FinderSearchEnded,
	#superclass : #Announcement,
	#instVars : [
		'results'
	],
	#category : #'Tool-Finder-Announcements'
}

{ #category : #accessing }
FinderSearchEnded class >> results: anObject [

	^ self new results: anObject; yourself
]

{ #category : #accessing }
FinderSearchEnded >> results [

	^ results
]

{ #category : #accessing }
FinderSearchEnded >> results: anObject [

	results := anObject
]
