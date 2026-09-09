Class {
	#name : 'OupsDebuggerSelectionStrategyTest',
	#superclass : 'TestCase',
	#category : 'Debugger-Oups-Tests-Tests - strategies',
	#package : 'Debugger-Oups-Tests',
	#tag : 'Tests - strategies'
}

{ #category : 'running' }
OupsDebuggerSelectionStrategyTest >> setUp [
	
	super setUp.
	('Running: ' , testSelector asString , ' from ' , self class name) traceCr.
]

{ #category : 'tests - debugging' }
OupsDebuggerSelectionStrategyTest >> testHandled [
	self deny: OupsDebuggerSelectionStrategy new handled
]
