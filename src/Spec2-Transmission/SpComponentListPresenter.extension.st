Extension { #name : #SpComponentListPresenter }

{ #category : #'*Spec2-Transmission' }
SpComponentListPresenter >> defineInputPorts [
	
	^ { SpListItemsPort new }
]

{ #category : #'*Spec2-Transmission' }
SpComponentListPresenter >> defineOutputPorts [
	
	^ { 
	SpSelectionPort new. 
	SpActivationPort new 
	}
]
