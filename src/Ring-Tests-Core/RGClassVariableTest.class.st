Class {
	#name : #RGClassVariableTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGClassVariableTest >> testClassVariableImplicitEnvironment [

	| classVariable |
	
	classVariable := RGClassVariable unnamed.
	self assert: (classVariable hasResolvedName not).
	self checkImplicitSingleClassEnvironmentOf: classVariable parent.
	
	classVariable := RGClassVariable named: #SomeClassVariable.
	self assert: (classVariable hasResolvedName).
	self checkImplicitSingleClassEnvironmentOf: classVariable parent.
	
	
	

]

{ #category : #tests }
RGClassVariableTest >> testNewClassVariable [

	| classVariable | 
	classVariable  := RGClassVariable unnamed.
	self assert: (classVariable isRingResolved).
	self assert: (classVariable hasUnresolved: #name).
	self assert: (classVariable isVariableDefinition).
	self assert: (classVariable isClassVariable).	
	self deny: (classVariable isPoolVariable).	

	classVariable  := RGClassVariable named: #SomeClassVariable.
	self assert: (classVariable isRingResolved).
	self assert: (classVariable hasResolvedAll: #(name)).
	self assert: classVariable name equals: #SomeClassVariable.
	self assert: (classVariable isClassVariable).
	self assert: (classVariable isClassVariable).	
	
]

{ #category : #tests }
RGClassVariableTest >> testResolvingConsistency [

	| anObject |
	anObject := RGClassVariable unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]
