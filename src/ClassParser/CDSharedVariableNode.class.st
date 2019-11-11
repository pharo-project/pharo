Class {
	#name : #CDSharedVariableNode,
	#superclass : #CDSlotNode,
	#category : #'ClassParser-Model'
}

{ #category : #'instance creation' }
CDSharedVariableNode class >> slot: aSlot node: aNode [ 
	
	^ self new
		slot: aSlot;
		node: aNode;
		yourself
]

{ #category : #transforming }
CDSharedVariableNode >> asClassVariable [
	^ ClassVariable named: self name.
]

{ #category : #testing }
CDSharedVariableNode >> isClassVariable [
	"To be polymorphic to RB method nodes"
	^true
]

{ #category : #testing }
CDSharedVariableNode >> isInstance [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSharedVariableNode >> isVariable [
	"To be polymorphic to RB method nodes"
	| existingClass |
	existingClass := self classDefinitionNode existingClassIfAbsent: [
		"Until class will be created the variables does not exist yet"
		^false ].
	^existingClass hasClassVarNamed: name asSymbol
]
