"
! Font property (==SpStyleFont==)

Font properties control how a component (morph) with font will draw the text.
I keep this properties: 

- name: The font name (it needs to be available in the list of fonts, e.g. ""Source Code Pro"") 
- size: The font point size.
- bold: Font is bold? (boolean, default """"false"""")
- italic: Font is italic? (boolean, default """"false"""")

!! Usage
The identifier of font in the stylesheet is ==Font==.

[[[
Font { 
 	#name: ""Source Sans Pro"",  
	#size: 12,
	#bold: false,
	#italic: false
}
]]]
"
Class {
	#name : #SpStyleFont,
	#superclass : #SpStyleProperty,
	#instVars : [
		'name',
		'size',
		'italic',
		'bold'
	],
	#classVars : [
		'FontCache'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #private }
SpStyleFont class >> addFontToCache: aFont [

	self fontCache add: aFont
]

{ #category : #private }
SpStyleFont class >> fontCache [

	^ FontCache ifNil: [ FontCache := WeakSet new ]
]

{ #category : #'ston-core' }
SpStyleFont class >> stonName [
	
	^ 'Font'
]

{ #category : #private }
SpStyleFont >> addFontToCache: aFont [

	self class addFontToCache: aFont
]

{ #category : #operations }
SpStyleFont >> applyTo: aMorph [

	"not all morphs understand font, I neet to verify it (which is not cool verifying a 
	 selector but other way would be to define #font: in Morph and I do not want that)"
	(aMorph respondsTo: #font:) ifFalse: [ ^ self ].
	
	aMorph font: self definedFont
]

{ #category : #accessing }
SpStyleFont >> bold [
	^ bold
]

{ #category : #accessing }
SpStyleFont >> bold: anObject [
	bold := anObject
]

{ #category : #private }
SpStyleFont >> calculateDefinedFont [
	| font |
	
	font := LogicalFont
		familyName: self name 
		pointSize: self size.

	self isItalic ifTrue: [ font forceItalicOrOblique ].
	self isBold ifTrue: [ font forceBold ].

	^ font
]

{ #category : #private }
SpStyleFont >> definedFont [
	| definedFont |

	self withCachedFontDo: [ :aFont | ^ aFont ].
	
	definedFont := self calculateDefinedFont.
	self addFontToCache: definedFont.
	^ definedFont
]

{ #category : #initialization }
SpStyleFont >> isBold [
	"property may be nil, we verify with strict comparisson"
	
	^ bold == true 
]

{ #category : #initialization }
SpStyleFont >> isItalic [
	"property may be nil, we verify with strict comparisson"
	
	^ italic == true 
]

{ #category : #accessing }
SpStyleFont >> italic [
	^ italic
]

{ #category : #accessing }
SpStyleFont >> italic: anObject [
	italic := anObject
]

{ #category : #accessing }
SpStyleFont >> name [

	^ name
]

{ #category : #accessing }
SpStyleFont >> name: aString [

	name := aString
]

{ #category : #accessing }
SpStyleFont >> size [

	^ size
]

{ #category : #accessing }
SpStyleFont >> size: aNumber [

	size := aNumber
]

{ #category : #private }
SpStyleFont >> withCachedFontDo: aBlock [

	self class fontCache 
		detect: [ :each | 
			each familyName = self name 
				and: [ each pointSize = self size 
				and: [ each isBold = self isBold
				and: [ each isItalic = self isItalic ] ] ] ]
		ifFound: aBlock
]
