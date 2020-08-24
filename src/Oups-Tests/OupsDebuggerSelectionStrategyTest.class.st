Class {
	#name : #OupsDebuggerSelectionStrategyTest,
	#superclass : #TestCase,
	#category : #'Oups-Tests-tests - strategies'
}

{ #category : #'tests - debugging' }
OupsDebuggerSelectionStrategyTest >> testHandled [
	self deny: OupsDebuggerSelectionStrategy new handled
]
