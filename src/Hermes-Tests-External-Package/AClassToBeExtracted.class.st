Class {
	#name : #AClassToBeExtracted,
	#superclass : #Object,
	#traits : 'TComparable',
	#classTraits : 'TComparable classTrait',
	#instVars : [
		'aVariable'
	],
	#category : 'Hermes-Tests-External-Package'
}

{ #category : #comparing }
AClassToBeExtracted >> < aComparable [
	self aVariable < aComparable aVariable.
]

{ #category : #comparing }
AClassToBeExtracted >> = aComparable [ 
	(self species ~= aComparable species) ifTrue:[^false].
	^ self aVariable = aComparable aVariable.
]

{ #category : #accessing }
AClassToBeExtracted >> aVariable [
	^ aVariable
]

{ #category : #accessing }
AClassToBeExtracted >> aVariable: anObject [
	aVariable := anObject
]

{ #category : #comparing }
AClassToBeExtracted >> hash [
	^ self aVariable hash
]
