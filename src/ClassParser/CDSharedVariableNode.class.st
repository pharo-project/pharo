"
This class models definiton of Class Variables
"
Class {
	#name : #CDSharedVariableNode,
	#superclass : #CDVariableNode,
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
	"for not this only supports ClassVariables.
	We need to create a ClassVariable here using the Variable Definition"
	^ ClassVariable named: self name
]

{ #category : #testing }
CDSharedVariableNode >> isClassVariable [
	"To be polymorphic to RB method nodes"
	^true
]

{ #category : #testing }
CDSharedVariableNode >> isGlobalClassNameBinding [
	"To be polymorphic to RB method nodes. This is a bit odd as this is just called due to
	#binding returning self, normally this should be called on LiteralVariable"

	^false
]

{ #category : #testing }
CDSharedVariableNode >> isInstanceVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSharedVariableNode >> isLiteralVariable [
	"To be polymorphic to RB method nodes"
	^true
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
