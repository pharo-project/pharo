Class {
	#name : #FFIFunctionResolutionTest,
	#superclass : #TestCase,
	#instVars : [
		'stack'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #helpers }
FFIFunctionResolutionTest >> pushLiteral: aLiteral [

	stack push: aLiteral
]

{ #category : #running }
FFIFunctionResolutionTest >> setUp [

	super setUp.
	stack := Stack new.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantIntegerShouldSetConstantLoader [

	| argument |
	argument := FFIValueArgument new
		value: 1;
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: 1.
]
