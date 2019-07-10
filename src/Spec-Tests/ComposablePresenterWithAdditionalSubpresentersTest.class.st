Class {
	#name : #ComposablePresenterWithAdditionalSubpresentersTest,
	#superclass : #TestCase,
	#instVars : [
		'application'
	],
	#category : #'Spec-Tests-Core'
}

{ #category : #running }
ComposablePresenterWithAdditionalSubpresentersTest >> setUp [
	super setUp.
	application := MockApplication new
]

{ #category : #tests }
ComposablePresenterWithAdditionalSubpresentersTest >> testOpening [
	| aPresenter |
	aPresenter := application newPresenter: TestingComposablePresenterWithAdditionalPresenters.
	aPresenter openWithSpec.

	#(subpresenter1 subpresenter2 subpresenter3) do: [ :aKey | self assert: (aPresenter additionalSubpresentersMap at: aKey) owner equals: aPresenter ]
]
