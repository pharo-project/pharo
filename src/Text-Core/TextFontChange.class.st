"
A TextFontChange encodes a font change applicable over a given range of text.  
The font number is interpreted relative to the textStyle governing display of this text.
"
Class {
	#name : #TextFontChange,
	#superclass : #TextAttribute,
	#instVars : [
		'fontNumber'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #private }
TextFontChange class >> defaultFontChange [
  "Answer a TextFontChange that represents the default font"

  | defaultFontStyle fontIndex |

  defaultFontStyle := TextStyle default.
  fontIndex := defaultFontStyle ifNil: [1] ifNotNil: [defaultFontStyle defaultFontIndex].

  ^ self new fontNumber: fontIndex.

]

{ #category : #defaults }
TextFontChange class >> font1 [
	^ self new fontNumber: 1
]

{ #category : #defaults }
TextFontChange class >> font2 [
	^ self new fontNumber: 2
]

{ #category : #defaults }
TextFontChange class >> font3 [
	^ self new fontNumber: 3
]

{ #category : #defaults }
TextFontChange class >> font4 [
	^ self new fontNumber: 4
]

{ #category : #'instance creation' }
TextFontChange class >> fontNumber: n [
	^ self new fontNumber: n
]

{ #category : #comparing }
TextFontChange >> = other [ 
	^ (other class == self class) 
		and: [other fontNumber = fontNumber]
]

{ #category : #accessing }
TextFontChange >> dominates: other [
	^ other isKindOf: TextFontChange
]

{ #category : #accessing }
TextFontChange >> fontNumber [
	^ fontNumber
]

{ #category : #accessing }
TextFontChange >> fontNumber: int [
	fontNumber := int
]

{ #category : #styling }
TextFontChange >> forFontInStyle: aTextStyle do: aBlock [
	aBlock value: (aTextStyle fontAt: fontNumber)
]

{ #category : #comparing }
TextFontChange >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^fontNumber hash
]

{ #category : #printing }
TextFontChange >> printOn: strm [
	super printOn: strm.
	strm nextPutAll: ' font: '; print: fontNumber
]
