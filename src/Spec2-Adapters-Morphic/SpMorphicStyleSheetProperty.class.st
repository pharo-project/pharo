"
I'm a base property for stylesheets.
I have a name (something that identifies me), and I can be applies to a morph (see #applyTo:)
"
Class {
	#name : #SpMorphicStyleSheetProperty,
	#superclass : #Object,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #operations }
SpMorphicStyleSheetProperty >> applyTo: aMorph [
]

{ #category : #accessing }
SpMorphicStyleSheetProperty >> name [

	^ self subclassResponsibility
]
