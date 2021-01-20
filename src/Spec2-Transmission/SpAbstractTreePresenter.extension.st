Extension { #name : #SpAbstractTreePresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractTreePresenter >> defaultInputPort [

	^ self inputRootsPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTreePresenter >> defaultOutputPort [

	^ self outputSelectionPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTreePresenter >> inputRootsPort [

	^ SpRootsPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTreePresenter >> outputActivationPort [
	
	^ SpActivationPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTreePresenter >> outputSelectionPort [
	
	^ SpSelectionPort newPresenter: self
]
