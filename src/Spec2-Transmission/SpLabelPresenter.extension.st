Extension { #name : #SpLabelPresenter }

{ #category : #'*Spec2-Transmission' }
SpLabelPresenter >> defaultInputPort [

	^ self inputLabelPort
]

{ #category : #'*Spec2-Transmission' }
SpLabelPresenter >> inputLabelPort [

	^ SpLabelPort newPresenter: self
]
