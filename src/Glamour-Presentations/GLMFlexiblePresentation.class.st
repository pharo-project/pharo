"
A presentation that flexibly changes it behavior depending on the current entity.

If the entity is a collection, this class renders as a ListPresentation, otherwise as a TextPresentation.
"
Class {
	#name : #GLMFlexiblePresentation,
	#superclass : #GLMPresentation,
	#category : #Glamour-Presentations
}

{ #category : #accessing }
GLMFlexiblePresentation >> currentPresentation [ 
	 
	^ (self displayValue isCollection and: [self displayValue isString not])
		ifTrue: [GLMListPresentation new showOnly: 50; pane: self pane]
		ifFalse: [GLMRubricTextPresentation new pane: self pane]
]

{ #category : #rendering }
GLMFlexiblePresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements. 
	^self currentPresentation renderGlamorouslyOn: aRenderer
]
