"
I'm a property to define the font of a morph. 
Since fonts can ne affected by scale and others, I receive a provider (a block) and not a font directly.
"
Class {
	#name : #SpMorphicStyleSheetFont,
	#superclass : #SpMorphicStyleSheetProperty,
	#instVars : [
		'providerBlock'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'instance creation' }
SpMorphicStyleSheetFont class >> provider: aBlock [

	^ self new 
		provider: aBlock;
		yourself
]

{ #category : #operations }
SpMorphicStyleSheetFont >> applyTo: aMorph [

	aMorph font: providerBlock value
]

{ #category : #accessing }
SpMorphicStyleSheetFont >> name [

	^ #font
]

{ #category : #accessing }
SpMorphicStyleSheetFont >> provider: aBlock [

	providerBlock := aBlock
]
