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
	#category : 'OpalCompiler-Tests-FromOld'
}

{ #category : #'as yet unclassified' }
MockSourceEditor >> contents: aText [

	text := aText
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> cut [

	text := (text first: selectionStart-1), (text copyFrom: selectionEnd+1 to: text size).
		
	selectionStart := selectionStart - 1.	
	selectionEnd := selectionStart
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> editor [

	^ self
]

{ #category : #initialization }
MockSourceEditor >> initialize [

	super initialize.
	selectionStart := selectionEnd := 0.
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> insertAndSelect: aString at: anInteger [

	text := (text first: anInteger-1), 
		(' ' , aString), 
		(text copyFrom: anInteger to: text size).
		
	selectionStart := anInteger.
	selectionEnd := anInteger + aString size 
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> notify: aString at: anInteger in: aStream [

	| pos |
	pos := self selectionInterval notEmpty
		ifTrue: [selectionStart + anInteger - 1 ]
		ifFalse: [anInteger].
	self insertAndSelect: aString at: (pos max: 1)
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> selectFrom: start to: end [

	selectionStart := start.
	selectionEnd := end.

]

{ #category : #'as yet unclassified' }
MockSourceEditor >> selection [

	^ self text copyFrom: selectionStart to: selectionEnd
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> selectionAsStream [

	^ ReadStream
		on: (self text copyFrom: selectionStart to: selectionEnd)
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> selectionInterval [

	^ Interval from: selectionStart to: selectionEnd.
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> startIndex [

	^ selectionStart
]

{ #category : #'as yet unclassified' }
MockSourceEditor >> text [

	^ text
]
