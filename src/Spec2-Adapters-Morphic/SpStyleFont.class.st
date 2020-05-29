"
! Font property (SpStyleFont)

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
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleFont class >> stonName [
	
	^ 'Font'
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

{ #category : #operations }
SpStyleFont >> definedFont [
	| definedFont |

	definedFont := LogicalFont
		familyName: self name 
		pointSize: self size.
		
	self isItalic ifTrue: [ definedFont forceItalicOrOblique ].
	self isBold ifTrue: [ definedFont forceBold ].

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
