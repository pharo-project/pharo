Class {
	#name : #FLBinaryFileStreamBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLBinaryFileStreamBasicSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLBinaryFileStreamStrategy new
]
