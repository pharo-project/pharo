"
THIS CLASS IS DEPRECATED, USE MY PARENT INSTEAD
"
Class {
	#name : #FFIExternalObject,
	#superclass : #FFIExternalPointer,
	#category : #Deprecated90
}

{ #category : #testing }
FFIExternalObject class >> isDeprecated [

	^ self = FFIExternalObject
]
