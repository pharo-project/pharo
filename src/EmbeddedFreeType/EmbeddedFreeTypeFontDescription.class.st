"
Common superclass handling fonts that are not found on the disc but embedded in the image.

Subclasses should redefine fontContents and originalFileName class methods. 



To do in the future:
	- a proper way to load font file and install them to avoid bloating the image with text that represent binary font data in addition to font themselves!!
"
Class {
	#name : #EmbeddedFreeTypeFontDescription,
	#superclass : #Object,
	#category : #'EmbeddedFreeType-Base'
}

{ #category : #testing }
EmbeddedFreeTypeFontDescription class >> canBeInstalled [
	"By default, font can be installed anywhere"
	^ true
		
]

{ #category : #accessing }
EmbeddedFreeTypeFontDescription class >> fontContents [
	self subclassResponsibility

]

{ #category : #accessing }
EmbeddedFreeTypeFontDescription class >> installAllFontsIn: provider [
	self allSubclasses do: [ :each | each installFontsIn: provider ]
]

{ #category : #accessing }
EmbeddedFreeTypeFontDescription class >> installFontsIn: provider [
	self canBeInstalled ifFalse: [ ^ self ].
	provider
		addFromFileContents: self fontContents 
		baseName: self originalFileName 
]

{ #category : #testing }
EmbeddedFreeTypeFontDescription class >> isAbstract [ 
	^self name = #EmbeddedFreeTypeFontDescription 
	
]

{ #category : #accessing }
EmbeddedFreeTypeFontDescription class >> originalFileName [
	self subclassResponsibility

]
