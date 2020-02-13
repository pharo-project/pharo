"
Renderer for font glyphs
"
Class {
	#name : #GlyphRenderer,
	#superclass : #Object,
	#category : #'Athens-Text'
}

{ #category : #'as yet unclassified' }
GlyphRenderer class >> forFont: aFont [
	^ self basicNew initForFont: aFont
]

{ #category : #'private - initialization' }
GlyphRenderer >> initForFont: aFont [

	self subclassResponsibility 
]
