Class {
	#name : #OupsDebuggerSelectionStrategyTest,
	#superclass : #TestCase,
	#category : #'Debugger-Oups-Tests-tests - strategies'
}

{ #category : #'tests - debugging' }
OupsDebuggerSelectionStrategyTest >> testHandled [
	self deny: OupsDebuggerSelectionStrategy new handled
]
