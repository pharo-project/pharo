Extension { #name : #SpMorphPresenter }

{ #category : #'*Spec2-Transmission' }
SpMorphPresenter >> defaultInputPort [

	^ self inputMorphPort
]

{ #category : #'*Spec2-Transmission' }
SpMorphPresenter >> inputMorphPort [ 

	^ SpMorphPort newPresenter: self
]
