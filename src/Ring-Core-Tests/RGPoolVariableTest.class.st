Class {
	#name : 'RGPoolVariableTest',
	#superclass : 'RGTest',
	#category : 'Ring-Core-Tests',
	#package : 'Ring-Core-Tests'
}

{ #category : 'tests' }
RGPoolVariableTest >> testPoolVariableImplicitEnvironment [

	| poolVariable |

	poolVariable := RGPoolVariable unnamed.
	self assert: (poolVariable hasUnresolved: #name).
	self assert: (poolVariable parent isClass).
	self checkImplicitSingleClassEnvironmentOf: poolVariable parent
]

{ #category : 'tests' }
RGPoolVariableTest >> testResolvingConsistency [

	| anObject |
	anObject := RGPoolVariable unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]
