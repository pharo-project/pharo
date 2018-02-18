Class {
	#name : #FLBinaryFileStreamSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core'
}

{ #category : #running }
FLBinaryFileStreamSerializationTest >> setUp [
	super setUp.
	self useBinaryFileStream

]
