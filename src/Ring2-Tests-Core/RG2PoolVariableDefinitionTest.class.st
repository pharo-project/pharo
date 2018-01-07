Class {
	#name : #RG2PoolVariableDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2PoolVariableDefinitionTest >> testPoolVariableImplicitEnvironment [

	| poolVariable |
	
	poolVariable := RG2PoolVariableDefinition unnamed.
	self assert: (poolVariable hasUnresolved: #name).
	self assert: (poolVariable parent isClass).
	self checkImplicitSingleClassEnvironmentOf: poolVariable parent.
	
	
	
	

]
