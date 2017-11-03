"
I am event indicating to the renderer to execute the  highlighted text and perform an action with the result.
"
Class {
	#name : #GLMHighlightAndEvaluate,
	#superclass : #Announcement,
	#instVars : [
		'action'
	],
	#category : #Glamour-Presentations-Announcements
}

{ #category : #accessing }
GLMHighlightAndEvaluate >> action [
	^ action
]

{ #category : #accessing }
GLMHighlightAndEvaluate >> action: anObject [
	action := anObject
]
