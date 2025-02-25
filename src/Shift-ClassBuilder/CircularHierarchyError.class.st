"
I am signaled if a new class introduces a circular class hierarchy.
"
Class {
	#name : 'CircularHierarchyError',
	#superclass : 'Error',
	#category : 'Shift-ClassBuilder-Exceptions',
	#package : 'Shift-ClassBuilder',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
CircularHierarchyError class >> signalFor: aClass [
	self signal: aClass name asString, ': Trying to build a circular hierarchy'
]
