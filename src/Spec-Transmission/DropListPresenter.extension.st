Extension { #name : #DropListPresenter }

{ #category : #'*Spec-Transmission' }
DropListPresenter >> defineInputPorts [

	^ { SpecListItemsPort new }
]

{ #category : #'*Spec-Transmission' }
DropListPresenter >> defineOutputPorts [

	^ { SpecDropListSelectionPort new }
]
