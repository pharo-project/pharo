Extension { #name : #SpAbstractListPresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> defaultInputPort [

	^ self inputItemsPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> inputItemsPort [

	^ SpListItemsPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> outputActivationPort [
	
	^ SpActivationPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpAbstractListPresenter >> outputSelectionPort [
	
	^ SpSelectionPort newPresenter: self
]
