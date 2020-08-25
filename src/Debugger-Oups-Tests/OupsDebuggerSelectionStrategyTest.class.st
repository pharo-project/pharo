Class {
	#name : #OupsDebuggerSelectionStrategyTest,
	#superclass : #TestCase,
	#category : #'Debugger-Oups-Tests-Tests - strategies'
}

{ #category : #'tests - debugging' }
OupsDebuggerSelectionStrategyTest >> testHandled [
	self deny: OupsDebuggerSelectionStrategy new handled
]
