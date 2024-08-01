"
A MCRemoval represents the removal of an entity of a given snapshot.
"
Class {
	#name : 'MCRemoval',
	#superclass : 'MCPatchOperation',
	#instVars : [
		'definition'
	],
	#category : 'Monticello-Patching',
	#package : 'Monticello',
	#tag : 'Patching'
}

{ #category : 'instance creation' }
MCRemoval class >> of: aDefinition [
	^ self new intializeWithDefinition: aDefinition
]

{ #category : 'accessing' }
MCRemoval >> baseDefinition [
	^ definition
]

{ #category : 'accessing' }
MCRemoval >> basicApplyTo: anObject [
	anObject removeDefinition: definition
]

{ #category : 'accessing' }
MCRemoval >> definition [
	^ definition
]

{ #category : 'accessing' }
MCRemoval >> fromSource [
	^ definition diffSource
]

{ #category : 'initializing' }
MCRemoval >> intializeWithDefinition: aDefinition [
	definition := aDefinition
]

{ #category : 'accessing' }
MCRemoval >> inverse [
	^ MCAddition of: definition
]

{ #category : 'testing' }
MCRemoval >> isClassPatch [
	^definition isClassDefinition
]

{ #category : 'testing' }
MCRemoval >> isRemoval [
	^ true
]

{ #category : 'accessing' }
MCRemoval >> targetClass [
	^ definition actualClass
]

{ #category : 'accessing' }
MCRemoval >> targetDefinition [
	^ nil
]

{ #category : 'accessing' }
MCRemoval >> toSource [
	^ ''
]
