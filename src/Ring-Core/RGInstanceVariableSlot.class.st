Class {
	#name : #RGInstanceVariableSlot,
	#superclass : #RGIndexedSlot,
	#category : #'Ring-Core-Kernel'
}

{ #category : #testing }
RGInstanceVariableSlot >> isSpecial [

	^ false
]

{ #category : #testing }
RGInstanceVariableSlot >> needsFullDefinition [
	"I am just a backward compatible ivar slot and can use simple definitions.
	 Note: my subclasses need full definitions"

	^ self class ~= RGInstanceVariableSlot
]
