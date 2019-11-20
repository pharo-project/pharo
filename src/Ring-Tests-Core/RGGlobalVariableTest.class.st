Class {
	#name : #RGGlobalVariableTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGGlobalVariableTest >> testGlobalVariableImplicitEnvironment [
	| globalVariable |
	globalVariable := RGGlobalVariable unnamed.
	self assert: (globalVariable hasUnresolved: #name).
	self assert: globalVariable environment ask globalVariables size equals: 1.
	self assert: globalVariable environment ask globalVariables anyOne identicalTo: globalVariable
]

{ #category : #tests }
RGGlobalVariableTest >> testNewGlobalVariable [

	| classVariable | 
	classVariable  := RGGlobalVariable named: #SomeGlobalVariable..
	self assert: (classVariable isRingResolved).
	self assert: (classVariable hasResolvedName).
	self assert: (classVariable isGlobalVariable).


	
]

{ #category : #tests }
RGGlobalVariableTest >> testResolvingConsistency [

	| anObject |
	anObject := RGGlobalVariable unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]
