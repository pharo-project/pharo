Extension { #name : #TreeTablePresenter }

{ #category : #'*Spec-Transmission' }
TreeTablePresenter >> defineInputPorts [ 

	^ { RootsPresenterPort new }
]

{ #category : #'*Spec-Transmission' }
TreeTablePresenter >> defineOutputPorts [ 

	^ { SpecListSelectionPort new }
]
