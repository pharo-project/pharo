"
self open.

self openWithScrollbar.

self openWithBorderOfWidth: 1.
self openWithBorderOfWidth: 5.

self openWithScrollbarAndBorderOfWidth: 0.
self openWithScrollbarAndBorderOfWidth: 1.
self openWithScrollbarAndBorderOfWidth: 5.

self openAutoAccept.
"
Class {
	#name : #RubTextFieldExample,
	#superclass : #Object,
	#instVars : [
		'string',
		'announcer'
	],
	#category : #'Rubric-Editing-Widgets-Examples'
}

{ #category : #examples }
RubTextFieldExample class >> open [
	| st |
	st := self new newScrolledText.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld

]

{ #category : #examples }
RubTextFieldExample class >> openAutoAccept [
	| st model |
	model := self new.
	st := model newScrolledTextAutoAccept.
	model inspect.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld

]

{ #category : #examples }
RubTextFieldExample class >> openWithBorderOfWidth: anInteger [
	| st |
	st := self new newScrolledTextWithBorderOfWidth: anInteger.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld

]

{ #category : #examples }
RubTextFieldExample class >> openWithScrollbar [
	| st |
	st := self new newScrolledTextWithScrollbar.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld

]

{ #category : #examples }
RubTextFieldExample class >> openWithScrollbarAndBorderOfWidth: anInteger [
	| st |
	st := self new newScrolledTextWithScrollbarAndBorderOfWidth: anInteger.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld

]

{ #category : #examples }
RubTextFieldExample class >> spyAnnouncer [
	"Set it to true if you want to observe text area announcements"
	^ false
]

{ #category : #accessing }
RubTextFieldExample >> acceptString: aString [
	self string: aString.
	^ true
]

{ #category : #accessing }
RubTextFieldExample >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #'user interface' }
RubTextFieldExample >> getText [
	^ self string asText
]

{ #category : #menu }
RubTextFieldExample >> menu [
	^ nil
]

{ #category : #'user interface' }
RubTextFieldExample >> newScrolledText [
	| st |
	st := (RubTextFieldMorph on: self)
		withAdornment;
		withGhostText: 'Input field bla bla bla bla bla bla';
		yourself.
	st width: 300.
	st textArea forbidMenu.
	^ st
]

{ #category : #'user interface' }
RubTextFieldExample >> newScrolledTextAutoAccept [
	| st |
	st := (RubTextFieldMorph
		on: self)
		withAdornment;
		withGhostText: 'Input field bla bla bla bla bla bla';
		autoAccept: true;
		yourself.
	st width: 300.
	^st
]

{ #category : #'user interface' }
RubTextFieldExample >> newScrolledTextWithBorderOfWidth: anInteger [
	^ self newScrolledText borderWidth: anInteger.
]

{ #category : #'user interface' }
RubTextFieldExample >> newScrolledTextWithScrollbar [
	^ self newScrolledText hScrollbarShowAlways
]

{ #category : #'user interface' }
RubTextFieldExample >> newScrolledTextWithScrollbarAndBorderOfWidth: anInteger [
	^ self newScrolledText hScrollbarShowAlways; borderWidth: anInteger
]

{ #category : #'user interface' }
RubTextFieldExample >> setText: aText from: aView [ 
	^ self acceptString: aText string
]

{ #category : #accessing }
RubTextFieldExample >> string [
	^ string ifNil: [ string := '' ]
]

{ #category : #accessing }
RubTextFieldExample >> string: aString [
	string := aString
]
