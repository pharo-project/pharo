Class {
	#name : #ImagePresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
ImagePresenterTest >> classToTest [
	^ ImagePresenter
]

{ #category : #tests }
ImagePresenterTest >> testSwitchAutoScale [
	| autoState |
	autoState := presenter autoScale.
	presenter switchAutoscale.
	self assert: presenter autoScale equals: autoState not
]
