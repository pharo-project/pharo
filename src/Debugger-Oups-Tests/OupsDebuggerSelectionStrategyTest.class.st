Class {
	#name : 'OupsDebuggerSelectionStrategyTest',
	#superclass : 'TestCase',
	#category : 'Debugger-Oups-Tests-Tests - strategies',
	#package : 'Debugger-Oups-Tests',
	#tag : 'Tests - strategies'
}

{ #category : 'private' }
OupsDebuggerSelectionStrategyTest >> performTest [
	"Temporary printing to find more information about https://github.com/pharo-project/pharo/issues/12502"

	('Running: ' , testSelector asString , ' from ' , self class name) traceCr.
	self perform: testSelector asSymbol
]

{ #category : 'tests - debugging' }
OupsDebuggerSelectionStrategyTest >> testHandled [
	self deny: OupsDebuggerSelectionStrategy new handled
]
