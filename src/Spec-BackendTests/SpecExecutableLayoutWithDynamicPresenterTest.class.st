Class {
	#name : #SpecExecutableLayoutWithDynamicPresenterTest,
	#superclass : #AbstractLayoutTestCase,
	#category : #'Spec-BackendTests-Layout'
}

{ #category : #accessing }
SpecExecutableLayoutWithDynamicPresenterTest >> classToTest [
	^ DynamicMockPresenter
]

{ #category : #tests }
SpecExecutableLayoutWithDynamicPresenterTest >> testSuceedToOpenInstance [
	self shouldnt: [ self openInstance ] raise: Error
]
