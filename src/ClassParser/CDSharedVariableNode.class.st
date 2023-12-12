"
This class models definiton of Class Variables
"
Class {
	#name : 'CDSharedVariableNode',
	#superclass : 'CDVariableNode',
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'instance creation' }
CDSharedVariableNode class >> slot: aSlot node: aNode [

	^ self new
		slot: aSlot;
		node: aNode;
		yourself
]

{ #category : 'transforming' }
CDSharedVariableNode >> asClassVariable [

	"when parsing old style definitions, the node is just the string of all ivars"
	node isLiteralNode ifTrue: [ ^ ClassVariable named: name ]

	"As we controll the creation of the AST, it is safe to evaluate it. The
	only thing that can go wrong is that the class of the Variable is not yet loaded,
	we crate an UndefinedClassVariable for that case".
	^ [ node evaluate ] on: Error do: [ UndefinedClassVariable named: self name ast: node ]
]

{ #category : 'testing' }
CDSharedVariableNode >> isClassVariable [
	"To be polymorphic to RB method nodes"
	^true
]

{ #category : 'testing' }
CDSharedVariableNode >> isInstanceVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : 'testing' }
CDSharedVariableNode >> isLiteralVariable [
	"To be polymorphic to RB method nodes"
	^true
]

{ #category : 'testing' }
CDSharedVariableNode >> isVariable [
	"To be polymorphic to RB method nodes"
	| existingClass |
	existingClass := self classDefinitionNode existingClassIfAbsent: [
		"Until class will be created the variables does not exist yet"
		^false ].
	^existingClass hasClassVarNamed: name asSymbol
]
