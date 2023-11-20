"
This exception is raised when mutating a read-only object using mirror primitives.

I am signaled from MirrorPrimitive methods.
"
Class {
	#name : 'MirrorModificationForbidden',
	#superclass : 'ModificationForbidden',
	#category : 'ReflectionMirrors-Primitives-Errors',
	#package : 'ReflectionMirrors-Primitives',
	#tag : 'Errors'
}

{ #category : 'retrying' }
MirrorModificationForbidden >> retryModification [

	fieldIndex
		ifNotNil: [
			MirrorPrimitives perform: retrySelector withArguments: {
					object.
					fieldIndex.
					newValue } ]
		ifNil: [
			MirrorPrimitives perform: retrySelector withArguments: {
					object.
					newValue } ].
	self resumeUnchecked: newValue
]
