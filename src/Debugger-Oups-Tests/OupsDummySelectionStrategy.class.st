"
I am a dummy debugger selection strategy, used for testing
"
Class {
	#name : 'OupsDummySelectionStrategy',
	#superclass : 'OupsDebuggerSelectionStrategy',
	#category : 'Debugger-Oups-Tests-Utils',
	#package : 'Debugger-Oups-Tests',
	#tag : 'Utils'
}

{ #category : 'iterating' }
OupsDummySelectionStrategy >> nextDebugger [
	"This method is not used in the tests, but is defined as a subclass responsibility by DSDebuggerSelectionStrategy"
	self error: 'Should not be called'
]

{ #category : 'instance creation' }
OupsDummySelectionStrategy >> openDebuggerForSession: aDebugSession [
	"Do nothing, to avoid opening a debugger during tests"
	^ 1
]
