Class {
	#name : #EmbeddedFreeTypeFontDescription,
	#superclass : #Object,
	#category : #'EmbeddedFreeType-Base'
}

{ #category : #testing }
EmbeddedFreeTypeFontDescription class >> canBeInstalled [
	"By default, concrete font descriptions can be installed anywhere"
	^ self isAbstract not
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
	^self name = #EmbeddedFreeTypeFontFontDescription 
	
]

{ #category : #accessing }
EmbeddedFreeTypeFontDescription class >> originalFileName [
	self subclassResponsibility

]
