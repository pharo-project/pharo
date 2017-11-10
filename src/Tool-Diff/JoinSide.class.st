Class {
	#name : #JoinSide,
	#superclass : #Object,
	#instVars : [
		'range',
		'offset',
		'lineRange',
		'highlights',
		'color',
		'text'
	],
	#category : #Tool-Diff
}

{ #category : #accessing }
JoinSide >> color [
	"Answer the value of color"

	^ color
]

{ #category : #accessing }
JoinSide >> color: anObject [
	"Set the value of color"

	color := anObject
]

{ #category : #'as yet unclassified' }
JoinSide >> highlight [
	"Answer the primary highlight."

	^(self highlights ifEmpty: [^nil]) first
]

{ #category : #accessing }
JoinSide >> highlights [
	"Answer the value of highlights"

	^ highlights
]

{ #category : #accessing }
JoinSide >> highlights: anObject [
	"Set the value of highlights"

	highlights := anObject
]

{ #category : #initialization }
JoinSide >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		highlights: #();
		offset: 0@0;
		range: (1 to: 1);
		lineRange: (1 to: 0);
		color: Color yellow;
		text: ''
]

{ #category : #accessing }
JoinSide >> lineRange [
	"Answer the value of lineRange"

	^ lineRange
]

{ #category : #accessing }
JoinSide >> lineRange: anObject [
	"Set the value of lineRange"

	lineRange := anObject
]

{ #category : #accessing }
JoinSide >> offset [
	"Answer the value of offset"

	^ offset
]

{ #category : #accessing }
JoinSide >> offset: anObject [
	"Set the value of offset"

	offset := anObject
]

{ #category : #accessing }
JoinSide >> range [
	"Answer the value of range"

	^ range
]

{ #category : #accessing }
JoinSide >> range: anObject [
	"Set the value of range"

	range := anObject
]

{ #category : #accessing }
JoinSide >> text [
	"Answer the value of text"

	^ text
]

{ #category : #accessing }
JoinSide >> text: anObject [
	"Set the value of text"

	text := anObject
]
