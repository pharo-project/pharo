Extension { #name : #SpAbstractPresenter }

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> defaultInputPort [

	^ nil
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> defaultOutputPort [

	^ nil
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitDo: aBlock [

	^ self defaultOutputPort transmitDo: aBlock
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitDo: aBlock transform: transformBlock [
	
	^ self defaultOutputPort
		transmitDo: aBlock 
		transform: transformBlock
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitTo: aPresenter [
	
	^ self defaultOutputPort transmitTo: aPresenter defaultInputPort
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitTo: aPresenter transform: aValuable [
	
	^ self defaultOutputPort
		transmitTo: aPresenter defaultInputPort
		transform: aValuable
]

{ #category : #'*Spec2-Transmission' }
SpAbstractPresenter >> transmitTo: aPresenter transform: aValuable postTransmission: postValuable [
	
	^ self defaultOutputPort
		transmitTo: aPresenter defaultInputPort
		transform: aValuable 
		postTransmission: postValuable
]
