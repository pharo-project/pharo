Extension { #name : #SpDropListPresenter }

{ #category : #'*Spec2-Transmission' }
SpDropListPresenter >> defineInputPorts [

	^ { SpListItemsPort new }
]

{ #category : #'*Spec2-Transmission' }
SpDropListPresenter >> defineOutputPorts [

	^ { SpDropListSelectionPort new }
]
