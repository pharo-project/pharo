"
This is a strategy to capture the intended arrangement of multiple presentations of a composite presentation. The subclasses define the actual arrangement.
"
Class {
	#name : #GLMCompositeArrangement,
	#superclass : #Object,
	#instVars : [
		'composite'
	],
	#category : #Glamour-Core
}

{ #category : #'instance creation' }
GLMCompositeArrangement class >> of: aCompositePresentation [
	^ self new composite: aCompositePresentation; yourself
]

{ #category : #accessing }
GLMCompositeArrangement >> composite [
	^ composite
]

{ #category : #accessing }
GLMCompositeArrangement >> composite: anObject [
	composite := anObject
]

{ #category : #rendering }
GLMCompositeArrangement >> renderGlamorouslyOn: aRenderer [
	self subclassResponsibility
]
