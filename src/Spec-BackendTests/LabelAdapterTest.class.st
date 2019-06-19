"
SUnit tests for MorphicLabelAdapter
"
Class {
	#name : #LabelAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
LabelAdapterTest >> classToTest [
	^ LabelPresenter
]

{ #category : #tests }
LabelAdapterTest >> testSetLabelInPresenterSetsInInMorph [
	presenter label: 'something'.
	self assert: self adapter label equals: 'something'
]
