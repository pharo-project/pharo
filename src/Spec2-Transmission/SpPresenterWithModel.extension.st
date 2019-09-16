Extension { #name : #SpPresenterWithModel }

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> defineInputPorts [
	
	^ { SpModelPort new }
]

{ #category : #'*Spec2-Transmission' }
SpPresenterWithModel >> setModel: aDomainObject [

	^	self model: aDomainObject
]
