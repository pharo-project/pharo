Class {
	#name : #GLMTextChanged,
	#superclass : #Announcement,
	#instVars : [
		'text'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #testing }
GLMTextChanged class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMTextChanged >> text [
	^ text
]

{ #category : #accessing }
GLMTextChanged >> text: anObject [
	text := anObject
]
