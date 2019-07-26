"
A GLMListPresentation simply shows the given elements within a list.

Instance Variables
	tagsBlock:		<Object>
	tagsFilterBlock:		<Object>

tagsBlock
	- xxxxx

tagsFilterBlock
	- xxxxx

"
Class {
	#name : #GLMListPresentation,
	#superclass : #GLMListingPresentation,
	#traits : 'GLMTBlockTags',
	#classTraits : 'GLMTBlockTags classTrait',
	#instVars : [
		'tagsBlock',
		'tagsFilterBlock'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #validation }
GLMListPresentation >> defaultValidate: anObject on: aPort [

	"we only allow objects that are in the display value"

	aPort name == #selection
		ifFalse: [ ^ true ].
	self entity ifNil: [ ^ false ].
	^ self isMultiple
		ifTrue:
			[ anObject isCollection and: [ anObject allSatisfy: [ :each | self displayValue includes: each ] ] ]
		ifFalse: [ self displayValue includes: anObject ]
]

{ #category : #rendering }
GLMListPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderListPresentation: self
]

{ #category : #accessing }
GLMListPresentation >> tagsBlock [
	^ tagsBlock
]

{ #category : #scripting }
GLMListPresentation >> tagsBlock: anObject [
	tagsBlock := anObject
]

{ #category : #scripting }
GLMListPresentation >> tagsFilter: aBlock [
	tagsFilterBlock := aBlock 
]

{ #category : #accessing }
GLMListPresentation >> tagsFilterBlock [
	^ tagsFilterBlock
]
