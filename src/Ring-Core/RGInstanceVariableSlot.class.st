Class {
	#name : #RGInstanceVariableSlot,
	#superclass : #RGIndexedSlot,
	#category : #'Ring-Core-Kernel'
}

{ #category : #generated }
RGInstanceVariableSlot >> isSpecial [

	self
		deprecated: 'Use #needsFullDefinition instead'
		transformWith: '`@rec isSpecial' -> '`@rec needsFullDefinition'.
	^ self needsFullDefinition
]

{ #category : #testing }
RGInstanceVariableSlot >> needsFullDefinition [
	"I am just a backward compatible ivar slot and can use simple definitions.
	 Note: my subclasses need full definitions"

	^ self class ~= RGInstanceVariableSlot
]
