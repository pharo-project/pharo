"
Common superclass handling fonts that are not found on the disc but embedded in the image.

Subclasses should redefine fontContents and originalFileName class methods. 



To do in the future:
	- a proper way to load font file and install them to avoid bloating the image with text that represent binary font data in addition to font themselves!!
"
Class {
	#name : #EmbeddedFreeTypeFontFontDescription,
	#superclass : #Object,
	#category : #EmbeddedFreeType
}

{ #category : #testing }
EmbeddedFreeTypeFontFontDescription class >> canBeInstalled [
	"By default, font can be installed anywhere"
	^ true
		
]

{ #category : #accessing }
EmbeddedFreeTypeFontFontDescription class >> fontContents [
	self subclassResponsibility

]

{ #category : #accessing }
EmbeddedFreeTypeFontFontDescription class >> installAllFontsIn: provider [
	self allSubclasses do: [ :each | each installFontsIn: provider ]
]

{ #category : #accessing }
EmbeddedFreeTypeFontFontDescription class >> installFontsIn: provider [
	self canBeInstalled ifFalse: [ ^ self ].
	provider
		addFromFileContents: self fontContents 
		baseName: self originalFileName 
]

{ #category : #accessing }
EmbeddedFreeTypeFontFontDescription class >> originalFileName [
	self subclassResponsibility

]
