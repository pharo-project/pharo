Class {
	#name : #GLMSelectedTextChanged,
	#superclass : #Announcement,
	#instVars : [
		'start',
		'end',
		'selectedText'
	],
	#category : #Glamour-Morphic-Widgets
}

{ #category : #accessing }
GLMSelectedTextChanged >> end [
	^ end
]

{ #category : #accessing }
GLMSelectedTextChanged >> end: anObject [
	end := anObject
]

{ #category : #accessing }
GLMSelectedTextChanged >> interval [
	^ self start to: self end
]

{ #category : #accessing }
GLMSelectedTextChanged >> selectedText [
	^ selectedText
]

{ #category : #accessing }
GLMSelectedTextChanged >> selectedText: anObject [
	selectedText := anObject
]

{ #category : #accessing }
GLMSelectedTextChanged >> start [
	^ start
]

{ #category : #accessing }
GLMSelectedTextChanged >> start: anObject [
	start := anObject
]
