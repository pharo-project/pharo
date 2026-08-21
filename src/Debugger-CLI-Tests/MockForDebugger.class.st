Class {
	#name : 'MockForDebugger',
	#superclass : 'Object',
	#category : 'Debugger-CLI-Tests-Mock',
	#package : 'Debugger-CLI-Tests',
	#tag : 'Mock'
}

{ #category : 'mock' }
MockForDebugger >> methodForNavigationInACode [

	self methodToStepInto
]

{ #category : 'mock' }
MockForDebugger >> methodToPrintCode: arg1 withArgument: arg2 [
	"Fake method to test printCode"

	| a b |
	a := 42.
	b := a + 1.
	^ b * arg1
]

{ #category : 'mock' }
MockForDebugger >> methodToStepInto [

	^ 42
]

{ #category : 'mock' }
MockForDebugger >> methodeToPrintCode [
	"Fake method to test printCode"

	| a b |
	a := 42.
	b := a + 1.
	^ b * 2
]
