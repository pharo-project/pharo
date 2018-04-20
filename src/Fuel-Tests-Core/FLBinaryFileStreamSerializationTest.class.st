Class {
	#name : #FLBinaryFileStreamSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : 'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLBinaryFileStreamSerializationTest >> setUp [
	super setUp.
	self useBinaryFileStream

]
