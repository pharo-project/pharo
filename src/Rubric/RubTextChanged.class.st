"
I am raised when the text is modified
"
Class {
	#name : #RubTextChanged,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'start',
		'stop',
		'text'
	],
	#category : #Rubric-Editing-Announcements
}

{ #category : #'instance creation' }
RubTextChanged class >> from: start to: stop with: aText [
	^ self new from: start to: stop with: aText
]

{ #category : #accessing }
RubTextChanged >> from: startIndex to: stopIndex with: aText [
	self start: startIndex.
	self stop: stopIndex.
	self text: aText
]

{ #category : #accessing }
RubTextChanged >> start [
	^ start
]

{ #category : #accessing }
RubTextChanged >> start: anInteger [
	start := anInteger
]

{ #category : #accessing }
RubTextChanged >> stop [
	^ stop
]

{ #category : #accessing }
RubTextChanged >> stop: anInteger [
	stop := anInteger
]

{ #category : #accessing }
RubTextChanged >> text [
	^ text
]

{ #category : #accessing }
RubTextChanged >> text: aText [
	text := aText
]
