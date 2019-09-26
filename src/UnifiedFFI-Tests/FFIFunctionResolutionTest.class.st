Class {
	#name : #FFIFunctionResolutionTest,
	#superclass : #TestCase,
	#instVars : [
		'stack'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #helpers }
FFIFunctionResolutionTest >> pushInstVar: anInteger [ 
	
	stack push: {#ivar . anInteger}
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushLiteral: aLiteral [

	stack push: { #literal . aLiteral }
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushReceiver [
	
	stack push: #( self )
]

{ #category : #running }
FFIFunctionResolutionTest >> setUp [

	super setUp.
	stack := Stack new.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantFalseShouldSetConstantZeroLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'false';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . 0 }.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantIntegerShouldSetConstantLoader [

	| argument |
	argument := FFIValueArgument new
		value: 1;
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . 1}.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantNULLShouldSetConstantNullLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'NULL';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . ExternalAddress null}.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantNilShouldSetConstantNullLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'nil';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . ExternalAddress null }.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantSelfShouldSetConstantSelfLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'self';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: #(#self).
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantTrueShouldSetConstantOneLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'true';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . 1 }.
]
