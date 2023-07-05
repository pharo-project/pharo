Class {
	#name : #FLBinaryFileStreamBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Core-Tests-Base'
}

{ #category : #running }
FLBinaryFileStreamBasicSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLBinaryFileStreamStrategy new
]
