Class {
	#name : 'ClapDbgMockForDebugger',
	#superclass : 'Object',
	#category : 'Debugger-CLI-Tests-Mock',
	#package : 'Debugger-CLI-Tests',
	#tag : 'Mock'
}

{ #category : 'mock' }
ClapDbgMockForDebugger >> methodForNavigationInACode [

	| response |
	response := self methodToStepInto.

	^ response
]

{ #category : 'mock' }
ClapDbgMockForDebugger >> methodToPrintCode: arg1 withArgument: arg2 [
	"Fake method to test printCode"

	| a b |
	a := 42.
	b := a + 1.
	^ b * arg1
]

{ #category : 'mock' }
ClapDbgMockForDebugger >> methodToStepInto [

	^ 42
]

{ #category : 'mock' }
ClapDbgMockForDebugger >> methodWithBlockForStepThrough [

	| value |
	value := [ 1 + 2 ] value.
	^ value
]

{ #category : 'mock' }
ClapDbgMockForDebugger >> methodeToPrintCode [
	"Fake method to test printCode"

	| a b |
	a := 42.
	b := a + 1.
	^ b * 2
]
