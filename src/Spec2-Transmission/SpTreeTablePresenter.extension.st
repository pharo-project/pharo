Extension { #name : #SpTreeTablePresenter }

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> defineInputPorts [ 

	^ { SpRootsPresenterPort new }
]

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> defineOutputPorts [ 

	^ { 
	SpSelectionPort new.
	SpActivationPort new 
	}
]
