"
A ClyTextEditorToolMorphTest is a test class for testing the behavior of ClyTextEditorToolMorph
"
Class {
	#name : 'ClyTextEditorToolMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'tool'
	],
	#category : 'Calypso-Browser-Tests',
	#package : 'Calypso-Browser-Tests'
}

{ #category : 'running' }
ClyTextEditorToolMorphTest >> setUp [

	super setUp.
	tool := ClyTextEditorToolMorph new.

]

{ #category : 'tests' }
ClyTextEditorToolMorphTest >> testFindAnyStringIn [

	| expectedInterval resultInterval targetString |
	
	targetString := 'does not define itself'.
	expectedInterval := 1 to: 15.
	resultInterval := tool 
		findAnyString: #('does not define') 
		in: targetString.
	self assert: resultInterval equals: expectedInterval.

]
