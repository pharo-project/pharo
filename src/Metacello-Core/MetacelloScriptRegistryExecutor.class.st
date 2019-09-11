Class {
	#name : #MetacelloScriptRegistryExecutor,
	#superclass : #MetacelloScriptExecutor,
	#category : #'Metacello-Core-Scripts'
}

{ #category : #'actions api' }
MetacelloScriptRegistryExecutor >> prime [
]

{ #category : #'execution callback' }
MetacelloScriptRegistryExecutor >> projectSpecSelectBlock [
  ^ [ :projectSpec | projectSpec notNil ]
]

{ #category : #'actions api' }
MetacelloScriptRegistryExecutor >> remove [
]

{ #category : #initialization }
MetacelloScriptRegistryExecutor >> reset [
]
