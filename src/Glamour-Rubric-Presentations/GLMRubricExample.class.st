Class {
	#name : #GLMRubricExample,
	#superclass : #Model,
	#category : #'Glamour-Rubric-Presentations-Examples'
}

{ #category : #accessing }
GLMRubricExample >> iconProvider [
	" For 2.0/3.0 compatibility "
	^ (Smalltalk respondsTo: #ui)
		ifTrue: [ (Smalltalk perform: #ui) icons]
		ifFalse: [ UITheme current ]
]

{ #category : #accessing }
GLMRubricExample >> loremIpsum [
	^ String loremIpsum: 81233
]

{ #category : #opening }
GLMRubricExample >> open [
	self presentation openOn: self loremIpsum 
]
