"
I am an announcement to force the UI to refresh the status bar.
This is useful when the results are handled in a asynchronous way
"
Class {
	#name : #DTStatusUpdate,
	#superclass : #Announcement,
	#instVars : [
		'message'
	],
	#category : #'DrTests-Announcements'
}

{ #category : #'instance creation' }
DTStatusUpdate class >> message: aString [

	^ self new
		  message: aString;
		  yourself
]

{ #category : #accessing }
DTStatusUpdate >> message [

	^ message
]

{ #category : #accessing }
DTStatusUpdate >> message: anObject [
	message := anObject
]
