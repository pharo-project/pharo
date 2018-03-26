Class {
	#name : #FLStandardFileStreamSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : 'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLStandardFileStreamSerializationTest >> setUp [
	super setUp.
	self useStandardFileStream

]
