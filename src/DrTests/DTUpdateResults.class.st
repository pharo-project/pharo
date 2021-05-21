"
I am an announcement to force the UI to refresh the results.
This is useful when the results are handled in a asynchronous way
"
Class {
	#name : #DTUpdateResults,
	#superclass : #Announcement,
	#instVars : [
		'results'
	],
	#category : #'DrTests-Announcements'
}

{ #category : #accessing }
DTUpdateResults >> results [

	^ results
]

{ #category : #accessing }
DTUpdateResults >> results: anObject [

	results := anObject
]
