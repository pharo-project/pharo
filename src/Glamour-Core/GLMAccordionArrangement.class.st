Class {
	#name : #GLMAccordionArrangement,
	#superclass : #GLMCompositeArrangement,
	#category : 'Glamour-Core'
}

{ #category : #rendering }
GLMAccordionArrangement >> renderGlamorouslyOn: aRenderer [
	^ aRenderer renderAccordionCompositePresentation: self composite
]
