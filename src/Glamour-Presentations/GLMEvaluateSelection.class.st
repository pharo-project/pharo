"
I am event indicating to the renderer to execute the currently selected text and perform an action with the result.
"
Class {
	#name : #GLMEvaluateSelection,
	#superclass : #Announcement,
	#instVars : [
		'action',
		'isSyncronous'
	],
	#category : #'Glamour-Presentations-Announcements'
}

{ #category : #accessing }
GLMEvaluateSelection >> action [
	^ action
]

{ #category : #accessing }
GLMEvaluateSelection >> action: anObject [
	action := anObject
]

{ #category : #initialization }
GLMEvaluateSelection >> initialize [
	super initialize.
	isSyncronous := false
]

{ #category : #accessing }
GLMEvaluateSelection >> isSyncronous [
	^ isSyncronous
]

{ #category : #accessing }
GLMEvaluateSelection >> makeSyncronous [
	isSyncronous := true
]
