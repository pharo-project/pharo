Class {
	#name : #SpExecutableLayoutTest,
	#superclass : #SpAbstractLayoutTest,
	#category : #'Spec2-Backend-Tests-Layout'
}

{ #category : #accessing }
SpExecutableLayoutTest >> classToTest [
	^ SpMockDynamicPresenter
]

{ #category : #tests }
SpExecutableLayoutTest >> testCanDynamicallyRebuildPresenter [
	self openInstance.
	self assert: presenter label label equals: ''.
	self shouldnt: [ presenter selectFirstElement ] raise: Error.
	self assert: presenter label label equals: '1'
]

{ #category : #tests }
SpExecutableLayoutTest >> testRebuildPresenterBeforeOpeningWorks [
	self assert: presenter label label equals: ''.
	self
		shouldnt: [ presenter selectFirstElement.
			self openInstance ]
		raise: Error.
	self assert: presenter label label equals: '1'
]
