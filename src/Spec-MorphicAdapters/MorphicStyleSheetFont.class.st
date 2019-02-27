"
I'm a property to define the font of a morph. 
Since fonts can ne affected by scale and others, I receive a provider (a block) and not a font directly.
"
Class {
	#name : #MorphicStyleSheetFont,
	#superclass : #MorphicStyleSheetProperty,
	#instVars : [
		'providerBlock'
	],
	#category : #'Spec-MorphicAdapters-StyleSheet'
}

{ #category : #'instance creation' }
MorphicStyleSheetFont class >> provider: aBlock [

	^ self new 
		provider: aBlock;
		yourself
]

{ #category : #operations }
MorphicStyleSheetFont >> applyTo: aMorph [

	aMorph font: providerBlock value
]

{ #category : #accessing }
MorphicStyleSheetFont >> name [

	^ #font
]

{ #category : #accessing }
MorphicStyleSheetFont >> provider: aBlock [

	providerBlock := aBlock
]
