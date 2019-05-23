Class {
	#name : #SpecExecutableLayoutTest,
	#superclass : #AbstractLayoutTestCase,
	#category : #'Spec-BackendTests-Layout'
}

{ #category : #accessing }
SpecExecutableLayoutTest >> classToTest [
	^ MockDynamicPresenter
]

{ #category : #tests }
SpecExecutableLayoutTest >> testCanDynamicallyRebuildPresenter [
	self openInstance.
	self assert: presenter label label equals: ''.
	self shouldnt: [ presenter selectFirstElement ] raise: Error.
	self assert: presenter label label equals: '1'
]

{ #category : #tests }
SpecExecutableLayoutTest >> testRebuildPresenterBeforeOpeningWorks [
	self assert: presenter label label equals: ''.
	self
		shouldnt: [ presenter selectFirstElement.
			self openInstance ]
		raise: Error.
	self assert: presenter label label equals: '1'
]
