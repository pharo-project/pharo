"
This presentation offers a means to embed a Spec user interface within Glamour. 
"
Class {
	#name : #GLMSpecPresentation,
	#superclass : #GLMPresentation,
	#category : 'Glamour-SpecIntegration'
}

{ #category : #rendering }
GLMSpecPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderSpecPresentation: self
]
