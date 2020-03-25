Extension { #name : #SpAbstractTextPresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractTextPresenter >> defaultInputPort [

	^ self inputTextPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTextPresenter >> defaultOutputPort [

	^ self outputTextChangedPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTextPresenter >> inputTextPort [ 

	^ SpTextPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTextPresenter >> outputTextChangedPort [

	^ SpTextChangedPort newPresenter: self
]
