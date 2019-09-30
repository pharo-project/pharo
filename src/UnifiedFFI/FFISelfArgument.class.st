"
I represent a ""self"" argument. 
This means a call on the form: 

#(void function ( self ) )

To be able to pass an object as ""self "" it need to accomplish one of this conditions: 

- it has to be an object mappable to an atomic type (that means: a number, a char, a string, ...)
- it class needs to have an instance variable 'handle', who will be taken as the real value to transmit (usually, this will be a en ExternalAddress)
"
Class {
	#name : #FFISelfArgument,
	#superclass : #FFIFunctionArgumentLoader,
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #'emitting code' }
FFISelfArgument >> emitArgument: aBuilder context: aContext [
	aBuilder pushReceiver
]

{ #category : #'emitting code' }
FFISelfArgument >> emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName [
	"If argument is an object, I need to send value of instVar handle instead it self"
	aBuilder pushInstVar: (objectClass 
		instVarIndexFor: aName 
		ifAbsent: [ self error: 'No ', aName, ' instVar' ])
]

{ #category : #'private emitting code' }
FFISelfArgument >> emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName [

	aBuilder 
		pushReceiver;
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ])
]

{ #category : #private }
FFISelfArgument >> rolledPointerTempName [
	^ self rolledPointerTempNameFor: #self
]
