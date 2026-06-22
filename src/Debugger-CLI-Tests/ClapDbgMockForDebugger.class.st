Class {
	#name : 'ClapDbgMockForDebugger',
	#superclass : 'Object',
	#category : 'Debugger-CLI-Tests-Mock',
	#package : 'Debugger-CLI-Tests',
	#tag : 'Mock'
}

{ #category : 'mock' }
ClapDbgMockForDebugger >> methodForNavigationInACode [

	self methodToStepInto
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
ClapDbgMockForDebugger >> methodeToPrintCode [
	"Fake method to test printCode"

	| a b |
	a := 42.
	b := a + 1.
	^ b * 2
]
