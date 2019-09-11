Extension { #name : #AbstractListPresenter }

{ #category : #'*Spec-Transmission' }
AbstractListPresenter >> defineInputPorts [
	
	^ { SpecListItemsPort new }
]

{ #category : #'*Spec-Transmission' }
AbstractListPresenter >> defineOutputPorts [
	
	^ { SpecListSelectionPort new }
]
