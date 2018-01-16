Class {
	#name : #RG2ClassVariableDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2ClassVariableDefinitionTest >> testClassVariableImplicitEnvironment [

	| classVariable |
	
	classVariable := RG2ClassVariableDefinition unnamed.
	self assert: (classVariable hasUnresolved: #name).
	self checkImplicitSingleClassEnvironmentOf: classVariable parent.
	
	classVariable := RG2ClassVariableDefinition named: #SomeClassVariable.
	self assert: (classVariable hasResolved: #name).
	self checkImplicitSingleClassEnvironmentOf: classVariable parent.
	
	
	

]

{ #category : #tests }
RG2ClassVariableDefinitionTest >> testNewClassVariable [

	| classVariable | 
	classVariable  := RG2ClassVariableDefinition unnamed.
	self assert: (classVariable isRing2Resolved).
	self assert: (classVariable hasUnresolved: #name).
	self assert: (classVariable isVariableDefinition).
	self assert: (classVariable isClassVariable).	
	self deny: (classVariable isPoolVariable).	

	classVariable  := RG2ClassVariableDefinition named: #SomeClassVariable.
	self assert: (classVariable isRing2Resolved).
	self assert: (classVariable hasResolvedAll: #(name)).
	self assert: classVariable name equals: #SomeClassVariable.
	self assert: (classVariable isClassVariable).
	self assert: (classVariable isClassVariable).	
	
]
