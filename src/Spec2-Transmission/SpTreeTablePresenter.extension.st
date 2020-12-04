Extension { #name : #SpTreeTablePresenter }

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> defaultInputPort [

	^ self inputRootsPort
]

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> inputRootsPort [

	^ SpRootsPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> outputActivationPort [
	
	^ SpActivationPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpTreeTablePresenter >> outputSelectionPort [
	
	^ SpSelectionPort newPresenter: self
]
