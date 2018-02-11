"
I am signaled if a new class introduces a circular class hierarchy.
"
Class {
	#name : #CircularHierarchyError,
	#superclass : #ClassBuilderError,
	#category : #'Slot-Core-Exception'
}

{ #category : #signalling }
CircularHierarchyError class >> signalFor: aClass [
	self signal: aClass name asString, ': Trying to build a circular hierarchy'
]
