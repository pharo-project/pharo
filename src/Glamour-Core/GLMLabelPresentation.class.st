Class {
	#name : #GLMLabelPresentation,
	#superclass : #GLMFormatedPresentation,
	#category : #Glamour-Core
}

{ #category : #rendering }
GLMLabelPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderLabelPresentation: self
]
