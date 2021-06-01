"
A mock object mocking a source editor
"
Class {
	#name : #MockSourceEditor,
	#superclass : #Object,
	#instVars : [
		'text',
		'selectionStart',
		'selectionEnd'
	],
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #accessing }
MockSourceEditor >> contents: aText [

	text := aText
]

{ #category : #utilities }
MockSourceEditor >> cut [

	text := (text first: selectionStart-1), (text copyFrom: selectionEnd+1 to: text size).
		
	selectionStart := selectionStart - 1.	
	selectionEnd := selectionStart
]

{ #category : #accessing }
MockSourceEditor >> editor [

	^ self
]

{ #category : #initialization }
MockSourceEditor >> initialize [

	super initialize.
	selectionStart := selectionEnd := 0.
]

{ #category : #utilities }
MockSourceEditor >> insertAndSelect: aString at: anInteger [

	text := (text first: anInteger-1), 
		(' ' , aString), 
		(text copyFrom: anInteger to: text size).
		
	selectionStart := anInteger.
	selectionEnd := anInteger + aString size 
]

{ #category : #notifications }
MockSourceEditor >> notify: aString at: anInteger in: aStream [

	| pos |
	pos := self selectionInterval notEmpty
		ifTrue: [selectionStart + anInteger - 1 ]
		ifFalse: [anInteger].
	self insertAndSelect: aString at: (pos max: 1)
]

{ #category : #selection }
MockSourceEditor >> selectFrom: start to: end [

	selectionStart := start.
	selectionEnd := end.

]

{ #category : #selection }
MockSourceEditor >> selection [

	^ self text copyFrom: selectionStart to: selectionEnd
]

{ #category : #selection }
MockSourceEditor >> selectionAsStream [

	^ ReadStream
		on: (self text copyFrom: selectionStart to: selectionEnd)
]

{ #category : #selection }
MockSourceEditor >> selectionInterval [

	^ Interval from: selectionStart to: selectionEnd.
]

{ #category : #accessing }
MockSourceEditor >> startIndex [

	^ selectionStart
]

{ #category : #accessing }
MockSourceEditor >> text [

	^ text
]
