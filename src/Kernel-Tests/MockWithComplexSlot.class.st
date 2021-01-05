"
A mock with a more complex slot to be used by tests (to test class parser, class definition printer and other)
"
Class {
	#name : #MockWithComplexSlot,
	#superclass : #Object,
	#instVars : [
		'#aComplexSlot => ObservableSlot'
	],
	#category : #'Kernel-Tests-Fluid'
}

{ #category : #accessing }
MockWithComplexSlot >> aComplexSlot [

	^ aComplexSlot
]

{ #category : #initialization }
MockWithComplexSlot >> initialize [
	
	self class initializeSlots: self.
	super initialize.
]
