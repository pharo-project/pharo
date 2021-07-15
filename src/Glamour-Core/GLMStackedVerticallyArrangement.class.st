Class {
	#name : #GLMStackedVerticallyArrangement,
	#superclass : #GLMCompositeArrangement,
	#category : #'Glamour-Core'
}

{ #category : #testing }
GLMStackedVerticallyArrangement class >> isDeprecated [
	^ true
]

{ #category : #rendering }
GLMStackedVerticallyArrangement >> renderGlamorouslyOn: aRenderer [
	^ aRenderer renderStackedVerticallyCompositePresentation: self composite
]
