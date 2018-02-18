Class {
	#name : #FLStandardFileStreamSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core'
}

{ #category : #running }
FLStandardFileStreamSerializationTest >> setUp [
	super setUp.
	self useStandardFileStream

]
