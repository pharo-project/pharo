Class {
	#name : #MockWithComplexSlot,
	#superclass : #Object,
	#instVars : [
		'#aComplexSlot => PropertySlot'
	],
	#category : #'Kernel-Tests-Fluid'
}

{ #category : #initialization }
MockWithComplexSlot >> initialize [
	
	self class initializeSlots: self.
	super initialize.
]
