Class {
	#name : #FLFileReferenceStreamBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLFileReferenceStreamBasicSerializationTest >> setUp [
	super setUp.
	
	self useFileReferenceStream
]
