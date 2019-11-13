Extension { #name : #SpAbstractTextPresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractTextPresenter >> defineInputPorts [ 

	^ { SpTextPort new }
]

{ #category : #'*Spec2-Transmission' }
SpAbstractTextPresenter >> defineOutputPorts [

	^ { SpTextChangedPort new }
]
