Class {
	#name : #FLDelayedSerializerMock,
	#superclass : #FLSerializer,
	#category : #'FuelTests-Mocks'
}

{ #category : #protected }
FLDelayedSerializerMock >> setDefaultSerialization [

	^ serializationFactory := [:anObject :anEncoder | 
		(FLDelayedSerializationMock 
			with: anEncoder
			root: anObject 
			analyzer: self analyzer)
			run;
			yourself ]
]
