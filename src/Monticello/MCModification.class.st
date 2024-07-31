"
A MCModification represents the operation to modify an entity to a snapshot. 
"
Class {
	#name : 'MCModification',
	#superclass : 'MCPatchOperation',
	#instVars : [
		'obsoletion',
		'modification'
	],
	#category : 'Monticello-Patching',
	#package : 'Monticello',
	#tag : 'Patching'
}

{ #category : 'instance creation' }
MCModification class >> of: base to: target [
	^ self new initializeWithBase: base target: target
]

{ #category : 'accessing' }
MCModification >> baseDefinition [
	^ obsoletion
]

{ #category : 'accessing' }
MCModification >> basicApplyTo: anObject [
	anObject modifyDefinition: obsoletion to: modification
]

{ #category : 'accessing' }
MCModification >> definition [
	^ modification
]

{ #category : 'accessing' }
MCModification >> fromSource [
	^ obsoletion source
]

{ #category : 'initializing' }
MCModification >> initializeWithBase: base target: target [
	obsoletion := base.
	modification := target.
]

{ #category : 'accessing' }
MCModification >> inverse [
	^ MCModification of: modification to: obsoletion
]

{ #category : 'testing' }
MCModification >> isClassPatch [
	^obsoletion isClassDefinition
]

{ #category : 'testing' }
MCModification >> isModification [
	^ true
]

{ #category : 'accessing' }
MCModification >> modification [
	^ modification
]

{ #category : 'accessing' }
MCModification >> obsoletion [
	^ obsoletion
]

{ #category : 'accessing' }
MCModification >> targetClass [
	^ obsoletion actualClass
]

{ #category : 'accessing' }
MCModification >> targetDefinition [
	^ modification
]

{ #category : 'accessing' }
MCModification >> toSource [
	^ modification diffSource
]
