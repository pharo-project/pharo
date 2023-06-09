"
SUnit tests for basic in memory serialization
"
Class {
	#name : #FLByteArrayBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLByteArrayBasicSerializationTest >> setUpStreamStrategy [ 
	streamStrategy := FLByteArrayStreamStrategy new
]
