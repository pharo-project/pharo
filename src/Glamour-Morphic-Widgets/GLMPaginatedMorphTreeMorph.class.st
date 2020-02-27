Class {
	#name : #GLMPaginatedMorphTreeMorph,
	#superclass : #PaginatedMorphTreeMorph,
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #stepping }
GLMPaginatedMorphTreeMorph >> step [

	self window ifNil: [ ^ self ].
	self window isActive ifFalse: [ ^ self ].
	(self wantsSteps and: [ self model shouldDoStep ]) ifFalse: [ ^ self ].
	(self window boundsInWorld containsRect: self boundsInWorld) ifFalse: [ ^ self ].
	self model updateRoots.
	self updateList
]

{ #category : #stepping }
GLMPaginatedMorphTreeMorph >> stepTime [
	^ self model stepTime
]

{ #category : #stepping }
GLMPaginatedMorphTreeMorph >> wantsSteps [
	^ self model wantsSteps
]
