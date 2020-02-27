"
I am an abstract class representing a call-out argument, having a value and a type.
See my subclasses for the concrete kind of arguments.
"
Class {
	#name : #FFIFunctionArgument,
	#superclass : #Object,
	#instVars : [
		'loader',
		'resolvedType',
		'type'
	],
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #resolution }
FFIFunctionArgument >> asOldArraySpec [

	self subclassResponsibility
]

{ #category : #accessing }
FFIFunctionArgument >> defaultReturnOnError [

	^ self resolvedType defaultReturnOnError
]

{ #category : #'emitting code' }
FFIFunctionArgument >> emitArgument: anIRBuilder context: aContext inCallout: aCallout [

	self resolvedType emitArgument: anIRBuilder context: aContext inCallout: aCallout
]

{ #category : #resolution }
FFIFunctionArgument >> emitReturnArgument: anIRBuilder context: aContext [

	self resolvedType
		emitReturnArgument: anIRBuilder
		context: aContext
]

{ #category : #accessing }
FFIFunctionArgument >> externalTypeWithArity [
	
	^ self resolvedType externalTypeWithArity
]

{ #category : #accessing }
FFIFunctionArgument >> loader [
	
	^ loader
]

{ #category : #resolution }
FFIFunctionArgument >> resolveUsing: aResolver [

	self subclassResponsibility
]

{ #category : #accessing }
FFIFunctionArgument >> resolvedType [
	
	^ resolvedType
]

{ #category : #accessing }
FFIFunctionArgument >> type [
	
	^ type
]

{ #category : #accessing }
FFIFunctionArgument >> type: anObject [
	type := anObject
]
