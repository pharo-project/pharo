Class {
	#name : #SpMorphicMorphAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'currentMorph'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicMorphAdapter >> buildWidget [

	currentMorph := self presenter morph 
		ifNil: [ Morph new color: Color transparent ].

	self presenter whenMorphChangedDo: [ :morph | 
		self applyStyle: morph.
		currentMorph owner 
			ifNotNil: [ :ownerMorph | 
				ownerMorph replaceSubmorph: currentMorph by: morph ] ].
	
	^ currentMorph
]
