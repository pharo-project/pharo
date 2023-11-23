"
This is an example of a slot that has meta data which is set using a fluid api (cascade)
"
Class {
	#name : 'ExampleSlotWithFluidAPI',
	#superclass : 'InstanceVariableSlot',
	#instVars : [
		'value1',
		'value2'
	],
	#category : 'Slot-Examples-Base',
	#package : 'Slot-Examples',
	#tag : 'Base'
}

{ #category : 'printing' }
ExampleSlotWithFluidAPI >> printOn: aStream [
	aStream
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' value1: ';
		store: value1;
		nextPut: $;;
		nextPutAll: ' value2: ';
		store: value2
]

{ #category : 'accessing' }
ExampleSlotWithFluidAPI >> value1 [

	^ value1
]

{ #category : 'accessing' }
ExampleSlotWithFluidAPI >> value1: anObject [

	value1 := anObject
]

{ #category : 'accessing' }
ExampleSlotWithFluidAPI >> value2 [

	^ value2
]

{ #category : 'accessing' }
ExampleSlotWithFluidAPI >> value2: anObject [

	value2 := anObject
]
