Extension { #name : #SpPresenterWithModel }

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> defaultInputPort [

	^ self inputModelPort
]

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> inputModelPort [

	^ SpModelPort newPresenter: self
]

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> setModel: aDomainObject [

	^	self model: aDomainObject
]
