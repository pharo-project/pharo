Class {
	#name : #GLMStackedVerticallyArrangement,
	#superclass : #GLMCompositeArrangement,
	#category : #'Glamour-Core'
}

{ #category : #rendering }
GLMStackedVerticallyArrangement >> renderGlamorouslyOn: aRenderer [
	^ aRenderer renderStackedVerticallyCompositePresentation: self composite
]
