Class {
	#name : #ComposablePresenterWithAdditionalSubpresentersTest,
	#superclass : #TestCase,
	#category : #'Spec-Tests-Core'
}

{ #category : #tests }
ComposablePresenterWithAdditionalSubpresentersTest >> testOpening [ 

	SpecBindings value: SpecStubAdapterBindings during: [ 
		| aPresenter |
		aPresenter := TestingComposablePresenterWithAdditionalPresenters new.
		
		aPresenter openWithSpec.
	
		#(subpresenter1 subpresenter2 subpresenter3) do: [ :aKey |
			self assert: (aPresenter additionalSubpresentersMap at: aKey) owner equals: aPresenter ] ]
	
]
