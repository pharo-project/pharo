"
A MCAddition represents the operation to add an entity to a snapshot. 
"
Class {
	#name : #MCAddition,
	#superclass : #MCPatchOperation,
	#instVars : [
		'definition'
	],
	#category : 'Monticello-Patching'
}

{ #category : #instance-creation }
MCAddition class >> of: aDefinition [
	^ self new intializeWithDefinition: aDefinition
]

{ #category : #accessing }
MCAddition >> baseDefinition [
	^ nil
]

{ #category : #accessing }
MCAddition >> basicApplyTo: anObject [
	anObject addDefinition: definition
]

{ #category : #accessing }
MCAddition >> definition [
	^ definition
]

{ #category : #accessing }
MCAddition >> fromSource [
	^ ''
]

{ #category : #initializing }
MCAddition >> intializeWithDefinition: aDefinition [
	definition := aDefinition
]

{ #category : #accessing }
MCAddition >> inverse [
	^ MCRemoval of: definition
]

{ #category : #testing }
MCAddition >> isAddition [
	^ true
]

{ #category : #testing }
MCAddition >> isClassPatch [
	^definition isClassDefinition
]

{ #category : #accessing }
MCAddition >> targetClass [
	^definition actualClass 
]

{ #category : #accessing }
MCAddition >> targetDefinition [
	^ definition
]

{ #category : #accessing }
MCAddition >> toSource [
	^ definition diffSource
]
