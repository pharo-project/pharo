Class {
	#name : 'OCDoItVariableTest',
	#superclass : 'TestCase',
	#instVars : [
		'instVarForTest'
	],
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'helpers' }
OCDoItVariableTest >> readVarInDifferentContext: aVar [
	| temp |
	self assert: aVar name equals: #temp.

	temp := #thisTemp.
	^aVar readInContext: thisContext
]

{ #category : 'tests' }
OCDoItVariableTest >> testConvertingToDoItVariable [

	| temp var |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.

	self assert: (var asDoItVariableFrom: #anyContext) identicalTo: var
]

{ #category : 'tests' }
OCDoItVariableTest >> testCreationFromAnotherVariable [
	| temp var targetTemp |
	temp := 100.
	targetTemp := thisContext lookupVar: #temp.
	var := targetTemp asDoItVariableFrom: thisContext.

	self assert: var class equals: OCDoItVariable.
	self assert: var doItContext identicalTo: thisContext.
	self assert: var actualVariable identicalTo: targetTemp
]

{ #category : 'tests' }
OCDoItVariableTest >> testDoItCompilation [
	| temp var doIt |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.
	doIt := thisContext compiler
		source: 'temp + 2';
		isScripting: true;
		bindings: { var };
		compile.
	self assert: (doIt valueWithReceiver: self) equals: 102
]

{ #category : 'tests' }
OCDoItVariableTest >> testFromInstVarVariable [

	| var |
	instVarForTest := 100.
	var := OCDoItVariable named: #instVarForTest fromContext: thisContext.

	self assert: var name equals: #instVarForTest.
	self assert: var read equals: 100.
	var write: 200.
	self assert: var read equals: 200.
	self assert: instVarForTest equals: 200
]

{ #category : 'tests' }
OCDoItVariableTest >> testFromTempVariable [

	| temp var |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.

	self assert: var name equals: #temp.
	self assert: var read equals: 100.
	var write: 200.
	self assert: var read equals: 200.
	self assert: temp equals: 200
]

{ #category : 'tests' }
OCDoItVariableTest >> testReadCompilation [
	| temp var ast doIt |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.
	ast := [ temp + 2 ] sourceNode body asDoit doSemanticAnalysis.
	ast variableNodes first variable: var.
	doIt := ast generateMethod.

	self assert: (doIt valueWithReceiver: self) equals: 102
]

{ #category : 'tests' }
OCDoItVariableTest >> testReadInGivenContextShouldIgnoreIt [
	| temp var actual |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.

	actual := self readVarInDifferentContext: var.
	self assert: actual equals: 100
]

{ #category : 'tests' }
OCDoItVariableTest >> testUsingMethods [

	| temp var |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.

	self assert: var usingMethods equals: { thisContext method }
]

{ #category : 'tests' }
OCDoItVariableTest >> testWriteCompilation [
	| temp var ast doIt |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.
	ast := [ temp := 500 ] sourceNode body asDoit doSemanticAnalysis.
	ast variableNodes first variable: var.
	doIt := ast generateMethod.
	doIt valueWithReceiver: self.

	self assert: temp equals: 500
]

{ #category : 'tests' }
OCDoItVariableTest >> testWriteInGivenContextShouldIgnoreIt [
	| temp var |
	temp := 100.
	var := OCDoItVariable named: #temp fromContext: thisContext.

	self writeVarInDifferentContext: var value: 200.
	self assert: temp equals: 200
]

{ #category : 'helpers' }
OCDoItVariableTest >> writeVarInDifferentContext: aVar value: aValue [
	| temp |
	self assert: aVar name equals: #temp.

	temp := #thisTemp.
	aVar write: aValue inContext: thisContext.
	self assert: temp equals: #thisTemp
]
