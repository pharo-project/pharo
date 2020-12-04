Extension { #name : #SpTreePresenter }

{ #category : #'*Spec2-Transmission' }
SpTreePresenter >> defaultInputPort [

	^ self inputRootsPort
]

{ #category : #'*Spec2-Transmission' }
SpTreePresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : #'*Spec2-Transmission' }
SpTreePresenter >> inputRootsPort [

	^ SpRootsPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpTreePresenter >> outputActivationPort [
	
	^ SpActivationPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpTreePresenter >> outputSelectionPort [
	
	^ SpSelectionPort newPresenter: self
]
