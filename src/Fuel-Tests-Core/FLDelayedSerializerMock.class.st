Class {
	#name : #FLDelayedSerializerMock,
	#superclass : #FLSerializer,
	#category : 'Fuel-Tests-Core-Mocks'
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
