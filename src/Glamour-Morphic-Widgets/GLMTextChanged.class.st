Class {
	#name : #GLMTextChanged,
	#superclass : #Announcement,
	#instVars : [
		'text'
	],
	#category : #Glamour-Morphic-Widgets
}

{ #category : #accessing }
GLMTextChanged >> text [
	^ text
]

{ #category : #accessing }
GLMTextChanged >> text: anObject [
	text := anObject
]
