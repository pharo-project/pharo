Class {
	#name : 'RGInstanceVariableSlot',
	#superclass : 'RGIndexedSlot',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'testing' }
RGInstanceVariableSlot >> allowsShadowing [

	^ false
]

{ #category : 'testing' }
RGInstanceVariableSlot >> needsFullDefinition [
	"I am just a backward compatible ivar slot and can use simple definitions.
	 Note: my subclasses need full definitions"

	^ self class ~= RGInstanceVariableSlot
]
