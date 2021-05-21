"
SUnit tests for basic in memory serialization
"
Class {
	#name : #FLByteArrayBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #failures }
FLByteArrayBasicSerializationTest >> expectedFailures [
	^ super expectedFailures, #(testConsiderCustomWideSymbolGlobal)
]

{ #category : #running }
FLByteArrayBasicSerializationTest >> setUp [
	super setUp.
	
	self useByteArrayStream
]
