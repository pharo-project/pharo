"
A SearchEnded is an announcement raised when a search has ended.
"
Class {
	#name : #SearchEnded,
	#superclass : #Announcement,
	#instVars : [
		'results'
	],
	#category : #'Tool-Finder-Announcements'
}

{ #category : #accessing }
SearchEnded class >> results: anObject [

	^ self new results: anObject; yourself
]

{ #category : #accessing }
SearchEnded >> results [

	^ results
]

{ #category : #accessing }
SearchEnded >> results: anObject [

	results := anObject
]
