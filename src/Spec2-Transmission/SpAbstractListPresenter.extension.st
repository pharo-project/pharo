Extension { #name : #SpAbstractListPresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> defineInputPorts [
	
	^ { SpListItemsPort new }
]

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> defineOutputPorts [
	
	^ { 
	SpSelectionPort new. 
	SpActivationPort new 
	}
]
