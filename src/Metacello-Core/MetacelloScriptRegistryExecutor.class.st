Class {
	#name : 'MetacelloScriptRegistryExecutor',
	#superclass : 'MetacelloScriptExecutor',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'actions api' }
MetacelloScriptRegistryExecutor >> prime [
]

{ #category : 'execution callback' }
MetacelloScriptRegistryExecutor >> projectSpecSelectBlock [
  ^ [ :projectSpec | projectSpec isNotNil ]
]

{ #category : 'actions api' }
MetacelloScriptRegistryExecutor >> remove [
]

{ #category : 'initialization' }
MetacelloScriptRegistryExecutor >> reset [
]
