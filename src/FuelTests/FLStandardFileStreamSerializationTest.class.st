Class {
	#name : #FLStandardFileStreamSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #FuelTests
}

{ #category : #running }
FLStandardFileStreamSerializationTest >> setUp [
	super setUp.
	self useStandardFileStream

]
