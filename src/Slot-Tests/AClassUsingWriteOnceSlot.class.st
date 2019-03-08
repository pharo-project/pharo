"
Slot testing class.
"
Class {
	#name : #AClassUsingWriteOnceSlot,
	#superclass : #Object,
	#instVars : [
		'#x => WriteOnceSlot'
	],
	#category : #'Slot-Tests-Data'
}

{ #category : #initialization }
AClassUsingWriteOnceSlot >> initialize [
	super initialize.
	self class initializeSlots: self.
]

{ #category : #accessing }
AClassUsingWriteOnceSlot >> modify: value [
	x := value. 
	
]

{ #category : #accessing }
AClassUsingWriteOnceSlot >> read [ 	
	^ x 
]
