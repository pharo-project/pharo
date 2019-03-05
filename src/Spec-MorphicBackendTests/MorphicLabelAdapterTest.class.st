"
SUnit tests for MorphicLabelAdapter
"
Class {
	#name : #MorphicLabelAdapterTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #accessing }
MorphicLabelAdapterTest >> classToTest [
	^ LabelPresenter
]

{ #category : #tests }
MorphicLabelAdapterTest >> testSetLabelInPresenterSetsInInMorph [
	presenter label: 'something'.
	self assert: self widget contents equals: 'something'
]
