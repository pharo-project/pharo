Extension { #name : #SpDropListPresenter }

{ #category : #'*Spec2-Transmission' }
SpDropListPresenter >> defaultInputPort [

	^ self inputItemsPort
]

{ #category : #'*Spec2-Transmission' }
SpDropListPresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : #'*Spec2-Transmission' }
SpDropListPresenter >> inputItemsPort [

	^ SpListItemsPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpDropListPresenter >> outputSelectionPort [

	^ SpDropListSelectionPort newPresenter: self
]
