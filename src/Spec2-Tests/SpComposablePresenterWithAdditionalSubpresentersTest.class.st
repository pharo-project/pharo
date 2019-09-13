Class {
	#name : #SpComposablePresenterWithAdditionalSubpresentersTest,
	#superclass : #TestCase,
	#instVars : [
		'application'
	],
	#category : #'Spec2-Tests-Core'
}

{ #category : #running }
SpComposablePresenterWithAdditionalSubpresentersTest >> setUp [
	super setUp.
	application := SpMockApplication new
]

{ #category : #tests }
SpComposablePresenterWithAdditionalSubpresentersTest >> testOpening [
	| aPresenter |
	aPresenter := application newPresenter: SpTestingPresenterWithAdditionalPresenters.
	aPresenter openWithSpec.

	#(subpresenter1 subpresenter2 subpresenter3) do: [ :aKey | self assert: (aPresenter additionalSubpresentersMap at: aKey) owner equals: aPresenter ]
]
