"
A specialized form for glyphs
"
Class {
	#name : #GlyphForm,
	#superclass : #Form,
	#instVars : [
		'advance',
		'linearAdvance'
	],
	#category : 'FreeType-Fonts'
}

{ #category : #accessing }
GlyphForm >> advance [
	^advance
]

{ #category : #accessing }
GlyphForm >> advance: aNumber [
	^advance := aNumber
]

{ #category : #converting }
GlyphForm >> asFormOfDepth: d [
	| newForm |
	d = self depth ifTrue:[^self].
	newForm := self class extent: self extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (self colormapIfNeededFor: newForm);
		copy: (self boundingBox)
		from: 0@0 in: self
		fillColor: nil rule: Form over.
	newForm 
		offset: offset;
		advance:advance;
		linearAdvance: linearAdvance.
	^newForm
]

{ #category : #accessing }
GlyphForm >> linearAdvance [
	^linearAdvance

]

{ #category : #accessing }
GlyphForm >> linearAdvance: aNumber [
	^linearAdvance := aNumber
]
