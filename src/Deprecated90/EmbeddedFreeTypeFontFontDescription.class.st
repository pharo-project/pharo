"
Deprecated class, please use class `EmbeddedFreeTypeFontDescription`
"
Class {
	#name : #EmbeddedFreeTypeFontFontDescription,
	#superclass : #EmbeddedFreeTypeFontDescription,
	#category : #'Deprecated90-EmbeddedFreeType'
}

{ #category : #accessing }
EmbeddedFreeTypeFontFontDescription class >> fontContents [
	self subclassResponsibility

]

{ #category : #testing }
EmbeddedFreeTypeFontFontDescription class >> isDeprecated [ 

	 ^true
]

{ #category : #accessing }
EmbeddedFreeTypeFontFontDescription class >> originalFileName [
	self subclassResponsibility

]
