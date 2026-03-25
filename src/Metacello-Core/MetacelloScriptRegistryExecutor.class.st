Class {
	#name : 'MetacelloScriptRegistryExecutor',
	#superclass : 'MetacelloScriptExecutor',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'actions api' }
MetacelloScriptRegistryExecutor >> remove [
]

{ #category : 'initialization' }
MetacelloScriptRegistryExecutor >> reset [
]

{ #category : 'execution callback' }
MetacelloScriptRegistryExecutor >> shouldBeExecutedFor: projectSpec [

	^ projectSpec isNotNil
]
