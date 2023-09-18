Class {
	#name : 'FLBinaryFileStreamBasicSerializationTest',
	#superclass : 'FLBasicSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FLBinaryFileStreamBasicSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLBinaryFileStreamStrategy new
]
