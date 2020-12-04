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
	#category : #'FreeType-Fonts'
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

{ #category : #converting }
GlyphForm >> invertedAndAlphaMultiplied [
	| result |
	result := super invertedAndAlphaMultiplied.
	^ (self class extent: result extent depth: result depth bits: result bits)
		offset: offset;
		advance: advance;
		linearAdvance: linearAdvance;
		yourself
]

{ #category : #accessing }
GlyphForm >> linearAdvance [
	^linearAdvance

]

{ #category : #accessing }
GlyphForm >> linearAdvance: aNumber [
	^linearAdvance := aNumber
]

{ #category : #converting }
GlyphForm >> withAlphaExtractedFromSubpixelRendering [
	| result |
	"Take the maximum of r,g,b as the alpha. Then multiply by a."
	result := self collectColors: [:c |
		| r g b a|
		r := c red.
		g := c green.
		b := c blue.
		a := c alpha.
		a := ((r max: g) max: b) * a.
		
		Color r: r g: g b: b alpha: a
	].

	^ (self class extent: result extent depth: result depth bits: result bits)
		offset: offset;
		advance: advance;
		linearAdvance: linearAdvance;
		yourself
]
