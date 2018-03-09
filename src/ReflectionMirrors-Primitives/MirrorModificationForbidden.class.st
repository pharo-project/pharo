"
This exception is raised when mutating a read-only object using mirror primitives.

I am signaled from MirrorPrimitive methods.
"
Class {
	#name : #MirrorModificationForbidden,
	#superclass : #ModificationForbidden,
	#category : #'ReflectionMirrors-Primitives'
}

{ #category : #retrying }
MirrorModificationForbidden >> retryModification [
	fieldIndex notNil
		ifTrue: [ MirrorPrimitives perform: retrySelector withArguments: {object. fieldIndex. newValue}]
		ifFalse: [MirrorPrimitives perform: retrySelector withArguments: {object. newValue}].
	self resumeUnchecked: newValue
]
