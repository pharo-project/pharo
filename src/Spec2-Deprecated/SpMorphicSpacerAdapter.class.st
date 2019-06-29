Class {
	#name : #SpMorphicSpacerAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Deprecated'
}

{ #category : #deprecation }
SpMorphicSpacerAdapter class >> isDeprecated [
	^ true
]

{ #category : #factory }
SpMorphicSpacerAdapter >> buildWidget [
	^ Morph new
		color: Color transparent;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]
