"
I can add or remove or modify definitions. My main purpose is to generate monticello snapshot out of multiple snapshots or definitions by using my most used method #apply:to:.
"
Class {
	#name : #MCPatcher,
	#superclass : #Object,
	#instVars : [
		'definitions'
	],
	#category : #'Monticello-Patching'
}

{ #category : #public }
MCPatcher class >> apply: aPatch to: aSnapshot [
	| loader |
	loader := self snapshot: aSnapshot.
	aPatch applyTo: loader.
	^ loader patchedSnapshot
]

{ #category : #'instance creation' }
MCPatcher class >> snapshot: aSnapshot [
	^ self new initializeWithSnapshot: aSnapshot
]

{ #category : #adding }
MCPatcher >> addDefinition: aDefinition [
	definitions add: aDefinition
]

{ #category : #initialization }
MCPatcher >> initializeWithSnapshot: aSnapshot [
	definitions := MCDefinitionIndex definitions: aSnapshot definitions
]

{ #category : #operations }
MCPatcher >> modifyDefinition: baseDefinition to: targetDefinition [
	self addDefinition: targetDefinition
]

{ #category : #accessing }
MCPatcher >> patchedSnapshot [
	^ MCSnapshot fromDefinitions: definitions definitions
]

{ #category : #operations }
MCPatcher >> removeDefinition: aDefinition [
	definitions remove: aDefinition
]
