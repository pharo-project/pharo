Class {
	#name : #ForeignVariableTest,
	#superclass : #TestCase,
	#instVars : [
		'instVarForTest'
	],
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #helpers }
ForeignVariableTest >> readVarInDifferentContext: aVar [
	| temp | 
	self assert: aVar name equals: #temp.
	
	temp := #thisTemp.
	^aVar readInContext: thisContext 
]

{ #category : #tests }
ForeignVariableTest >> testConvertingToForeignVariable [

	| temp var |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.
	
	self assert: (var asForeignVariableFrom: #anyContext) identicalTo: var
]

{ #category : #tests }
ForeignVariableTest >> testCreationFromAnotherVariable [
	| temp var targetTemp |
	temp := 100.
	targetTemp := thisContext lookupVar: #temp.
	var := targetTemp asForeignVariableFrom: thisContext.
	
	self assert: var class equals: ForeignVariable.
	self assert: var foreignContext identicalTo: thisContext.
	self assert: var actualVariable identicalTo: targetTemp
]

{ #category : #tests }
ForeignVariableTest >> testFromInstVarVariable [

	| var |
	instVarForTest := 100.
	var := ForeignVariable named: #instVarForTest fromContext: thisContext.
	
	self assert: var name equals: #instVarForTest.
	self assert: var read equals: 100.
	var write: 200.
	self assert: var read equals: 200.
	self assert: instVarForTest equals: 200
]

{ #category : #tests }
ForeignVariableTest >> testFromTempVariable [

	| temp var |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.
	
	self assert: var name equals: #temp.
	self assert: var read equals: 100.
	var write: 200.
	self assert: var read equals: 200.
	self assert: temp equals: 200
]

{ #category : #tests }
ForeignVariableTest >> testReadCompilation [
	| temp var tempAST tempReadDoIt |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.
	tempAST := [ temp + 2 ] sourceNode body asDoit.
	tempAST variableNodes first variable: var.
	tempReadDoIt := tempAST generateWithSource.
	
	self assert: (tempReadDoIt valueWithReceiver: self arguments: #()) equals: 102
]

{ #category : #tests }
ForeignVariableTest >> testReadInGivenContextShouldIgnoreIt [
	| temp var actual |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.

	actual := self readVarInDifferentContext: var.
	self assert: actual equals: 100
]

{ #category : #tests }
ForeignVariableTest >> testUsingMethods [

	| temp var |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.
	
	self assert: var usingMethods equals: { thisContext method }
]

{ #category : #tests }
ForeignVariableTest >> testWriteCompilation [
	| temp var tempAST tempWriteDoIt |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.
	tempAST := [ temp := 500 ] sourceNode body asDoit.
	tempAST variableNodes first variable: var.
	tempWriteDoIt := tempAST generateWithSource.
	tempWriteDoIt valueWithReceiver: self arguments: #().
	
	self assert: temp equals: 500
]

{ #category : #tests }
ForeignVariableTest >> testWriteInGivenContextShouldIgnoreIt [
	| temp var |
	temp := 100.
	var := ForeignVariable named: #temp fromContext: thisContext.

	self writeVarInDifferentContext: var value: 200.
	self assert: temp equals: 200
]

{ #category : #helpers }
ForeignVariableTest >> writeVarInDifferentContext: aVar value: aValue [
	| temp | 
	self assert: aVar name equals: #temp.
	
	temp := #thisTemp.
	aVar write: aValue inContext: thisContext.
	self assert: temp equals: #thisTemp
]
