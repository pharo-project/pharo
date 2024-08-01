"
I represent an error produced during materialization when is detected a change in the bytecodes of a method serialized as global. 

This error was born when testing the materialization of a BlockClosure defined in a method that changed. The test produced a VM crash.
"
Class {
	#name : 'FLMethodChanged',
	#superclass : 'FLMaterializationError',
	#category : 'Fuel-Core-Errors',
	#package : 'Fuel-Core',
	#tag : 'Errors'
}

{ #category : 'signaling' }
FLMethodChanged class >> signalWith: aGlobalName and: aSelector [
	^ self signal: 'Method ', aGlobalName, '>>#', aSelector, ' changed its bytecodes.'
]

{ #category : 'signalling' }
FLMethodChanged class >> signalWithBlock: aBlockWithoutOuterContext [

	^ self signal: 'Clean block' , aBlockWithoutOuterContext
		  , ' cannot be materialized because its method was changed.'
]
