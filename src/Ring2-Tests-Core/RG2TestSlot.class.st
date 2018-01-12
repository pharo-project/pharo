"
a testing slot for testing of unknown slots
"
Class {
	#name : #RG2TestSlot,
	#superclass : #InstanceVariableSlot,
	#category : #'Ring2-Tests-Core-Support'
}

{ #category : #'as yet unclassified' }
RG2TestSlot class >> forTesting [

	
]

{ #category : #printing }
RG2TestSlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' forTesting'.
]
