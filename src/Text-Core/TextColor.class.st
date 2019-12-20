"
A TextColor encodes a text color change applicable over a given range of text.
"
Class {
	#name : #TextColor,
	#superclass : #TextAttribute,
	#instVars : [
		'color'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #constants }
TextColor class >> black [
	^ self new color: Color black
]

{ #category : #constants }
TextColor class >> blue [
	^ self new color: Color blue
]

{ #category : #'instance creation' }
TextColor class >> color: aColor [
	^ self new color: aColor
]

{ #category : #constants }
TextColor class >> cyan [
	^ self new color: Color cyan
]

{ #category : #constants }
TextColor class >> gray [
	^ self new color: Color gray
]

{ #category : #constants }
TextColor class >> green [
	^ self new color: Color green
]

{ #category : #constants }
TextColor class >> magenta [
	^ self new color: Color magenta
]

{ #category : #constants }
TextColor class >> red [
	^ self new color: Color red
]

{ #category : #'instance creation' }
TextColor class >> scanFrom: strm [
	"read a color in the funny format used by Text styles on files. c125000255 or cblue;"

	| r g b |
	strm peek isDigit
		ifTrue:
			[r := (strm next: 3) asNumber.
			g := (strm next: 3) asNumber.
			b := (strm next: 3) asNumber.
			^ self color: (Color r: r g: g b: b range: 255)].
	"A name of a color"
	^ self color: (Color perform: (strm upTo: $;) asSymbol)
]

{ #category : #constants }
TextColor class >> white [ 
	^ self new color: Color white
]

{ #category : #constants }
TextColor class >> yellow [
	^ self new color: Color yellow
]

{ #category : #comparing }
TextColor >> = other [ 
	^ (other class == self class) 
		and: [other color = color]
]

{ #category : #converting }
TextColor >> asColor [ 

	^ color
]

{ #category : #accessing }
TextColor >> color [
	^ color
]

{ #category : #accessing }
TextColor >> color: aColor [
	color := aColor
]

{ #category : #scanning }
TextColor >> dominates: other [
"Only one color attribute on a same portion of text."
	^ other class == self class
]

{ #category : #comparing }
TextColor >> hash [
	^ color hash
]

{ #category : #printing }
TextColor >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' code: '; print: color
]
