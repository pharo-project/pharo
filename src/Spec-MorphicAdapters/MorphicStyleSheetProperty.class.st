"
I'm a base property for stylesheets.
I have a name (something that identifies me), and I can be applies to a morph (see #applyTo:)
"
Class {
	#name : #MorphicStyleSheetProperty,
	#superclass : #Object,
	#category : #'Spec-MorphicAdapters-StyleSheet'
}

{ #category : #operations }
MorphicStyleSheetProperty >> applyTo: aMorph [
]

{ #category : #accessing }
MorphicStyleSheetProperty >> name [

	^ self subclassResponsibility
]
