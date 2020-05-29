Extension { #name : #SpPresenterWithModel }

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> defaultInputPort [

	^ self inputModelPort
]

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> inputModelPort [

	^ SpModelPort newPresenter: self
]
