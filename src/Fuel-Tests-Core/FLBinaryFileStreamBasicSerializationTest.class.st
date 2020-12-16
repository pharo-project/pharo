Class {
	#name : #FLBinaryFileStreamBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLBinaryFileStreamBasicSerializationTest >> setUp [
	super setUp.
	self useBinaryFileStream

]
