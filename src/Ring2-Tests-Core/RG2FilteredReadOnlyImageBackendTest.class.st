Class {
	#name : #RG2FilteredReadOnlyImageBackendTest,
	#superclass : #RG2BackendTest,
	#traits : 'TRG2ReadOnlyDefinitionTest',
	#classTraits : 'TRG2ReadOnlyDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2FilteredReadOnlyImageBackendTest >> backendClass [

	^ RG2EnvironmentBackend
]

{ #category : #tests }
RG2FilteredReadOnlyImageBackendTest >> testFiltering [

	| env |
	
	env := RG2Environment new.
	env backend: (RG2FilteredReadOnlyImageBackend for: env).
	
	self assert: (env ask classNamed: #Object) notNil.
	self assert: (env ask classNamed: #RG2Definition) isNil.
	
	
]
