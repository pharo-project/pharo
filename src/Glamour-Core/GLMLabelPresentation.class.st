Class {
	#name : #GLMLabelPresentation,
	#superclass : #GLMFormatedPresentation,
	#category : #'Glamour-Core'
}

{ #category : #testing }
GLMLabelPresentation class >> isDeprecated [
	^ true
]

{ #category : #rendering }
GLMLabelPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderLabelPresentation: self
]
