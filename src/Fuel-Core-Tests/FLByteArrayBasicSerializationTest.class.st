"
SUnit tests for basic in memory serialization
"
Class {
	#name : #FLByteArrayBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Core-Tests-Base'
}

{ #category : #running }
FLByteArrayBasicSerializationTest >> setUpStreamStrategy [ 
	streamStrategy := FLByteArrayStreamStrategy new
]
