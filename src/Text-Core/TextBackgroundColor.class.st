"
I am a text attribute representing background color of a text. I am used as any other TextAttribute.


"
Class {
	#name : #TextBackgroundColor,
	#superclass : #TextAttribute,
	#instVars : [
		'color'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #constants }
TextBackgroundColor class >> black [
	^ self new color: Color black
]

{ #category : #constants }
TextBackgroundColor class >> blue [
	^ self new color: Color blue
]

{ #category : #'instance creation' }
TextBackgroundColor class >> color: aColor [
	^ self new color: aColor
]

{ #category : #constants }
TextBackgroundColor class >> cyan [
	^ self new color: Color cyan
]

{ #category : #constants }
TextBackgroundColor class >> gray [
	^ self new color: Color gray
]

{ #category : #constants }
TextBackgroundColor class >> green [
	^ self new color: Color green
]

{ #category : #constants }
TextBackgroundColor class >> magenta [
	^ self new color: Color magenta
]

{ #category : #constants }
TextBackgroundColor class >> red [
	^ self new color: Color red
]

{ #category : #'instance creation' }
TextBackgroundColor class >> scanFrom: strm [
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
TextBackgroundColor class >> white [ 
	^ self new color: Color white
]

{ #category : #constants }
TextBackgroundColor class >> yellow [
	^ self new color: Color yellow
]

{ #category : #comparing }
TextBackgroundColor >> = other [ 
	^ (other class == self class) 
		and: [other color = color]
]

{ #category : #converting }
TextBackgroundColor >> asColor [ 

	^ color
]

{ #category : #accessing }
TextBackgroundColor >> color [
	^ color
]

{ #category : #accessing }
TextBackgroundColor >> color: aColor [
	color := aColor
]

{ #category : #scanning }
TextBackgroundColor >> dominates: other [
"Only one background color attribute on a same portion of text."
	^ other class == self class
]

{ #category : #comparing }
TextBackgroundColor >> hash [
	^ color hash
]
