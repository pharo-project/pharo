"
GLMDiffPresentation is meant to show the difference between two input text objects.

The convention is that the transformed entity should provide a collection with two elements.
"
Class {
	#name : #GLMDiffPresentation,
	#superclass : #GLMFormatedPresentation,
	#instVars : [
		'contextClass'
	],
	#category : 'Glamour-Presentations'
}

{ #category : #accessing }
GLMDiffPresentation >> contextClass [
	^ contextClass
]

{ #category : #accessing }
GLMDiffPresentation >> contextClass: aClassOrBlock [
	"the class of method which source is being diffed"

	contextClass := aClassOrBlock
]

{ #category : #'accessing values' }
GLMDiffPresentation >> contextClassValue [
	^ self contextClass glamourValue: self entity
]

{ #category : #rendering }
GLMDiffPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderDiffPresentation: self
]
