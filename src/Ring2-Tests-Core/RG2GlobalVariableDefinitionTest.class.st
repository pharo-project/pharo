Class {
	#name : #RG2GlobalVariableDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2GlobalVariableDefinitionTest >> testGlobalVariableImplicitEnvironment [

	| globalVariable |
	
	globalVariable := RG2GlobalVariableDefinition unnamed.
	self assert: (globalVariable hasUnresolved: #name).
	self assert: globalVariable environment ask globalVariables size equals: 1.
	self assert: (globalVariable environment ask globalVariables anyOne == globalVariable).
	
]

{ #category : #tests }
RG2GlobalVariableDefinitionTest >> testNewGlobalVariable [

	| classVariable | 
	classVariable  := RG2GlobalVariableDefinition named: #SomeGlobalVariable..
	self assert: (classVariable isRing2Resolved).
	self assert: (classVariable hasResolved: #name).
	self assert: (classVariable isGlobalVariable).


	
]
