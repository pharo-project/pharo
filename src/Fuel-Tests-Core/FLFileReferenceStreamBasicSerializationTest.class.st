Class {
	#name : #FLFileReferenceStreamBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLFileReferenceStreamBasicSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLFileReferenceStreamStrategy new
]
