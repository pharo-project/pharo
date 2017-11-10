Class {
	#name : #FLBinaryFileStreamSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #FuelTests
}

{ #category : #running }
FLBinaryFileStreamSerializationTest >> setUp [
	super setUp.
	self useBinaryFileStream

]
