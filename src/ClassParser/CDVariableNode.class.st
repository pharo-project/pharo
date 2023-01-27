"
Abstract superclass for Variable definitions: Slots (instance and class side) and ClassVariables (aka SharedVariables)
"
Class {
	#name : #CDVariableNode,
	#superclass : #CDNode,
	#instVars : [
		'node',
		'start',
		'stop',
		'name',
		'variableClassName'
	],
	#category : #'ClassParser-Model'
}

{ #category : #testing }
CDVariableNode class >> isAbstract [

	^ self == CDVariableNode
]

{ #category : #'instance creation' }
CDVariableNode class >> node: aNode name: aName slotClassName: aSymbol start: start stop: stop [

	^ self new
		node: aNode;
		name: aName;
		variableClassName: aSymbol;
		start: start;
		stop: stop;
		yourself
]

{ #category : #accessing }
CDVariableNode >> asCalypsoVariableOf: aClass [
	^ self subclassResponsibility
]

{ #category : #accessing }
CDVariableNode >> binding [
	"To be polymorphic to RB method nodes"
	^self
]

{ #category : #testing }
CDVariableNode >> isClassVariable [
	^ self subclassResponsibility
]

{ #category : #testing }
CDVariableNode >> isGlobalVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDVariableNode >> isInstanceVariable [
	^ self subclassResponsibility
]

{ #category : #testing }
CDVariableNode >> isLiteralVariable [
	^ self subclassResponsibility
]

{ #category : #testing }
CDVariableNode >> isTempVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDVariableNode >> isUndeclaredVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDVariableNode >> isVariable [
	"To be polymorphic to RB method nodes"
	| existingClass |
	existingClass := self classDefinitionNode existingClassIfAbsent: [
		"Until class will be created the variables does not exist yet"
		^false ].
	^existingClass
		slotNamed: name asSymbol
		ifFound: [true]
		ifNone: [
			"Until class will be compiled with new slot the new slot does not exist yet"
			false]
]

{ #category : #testing }
CDVariableNode >> isWorkspaceVariable [
	^ false
]

{ #category : #accessing }
CDVariableNode >> name [

	^ name
]

{ #category : #accessing }
CDVariableNode >> name: aString [

	name := aString
]

{ #category : #accessing }
CDVariableNode >> node [
	^node
]

{ #category : #accessing }
CDVariableNode >> node: aNode [
	node := aNode
]

{ #category : #printing }
CDVariableNode >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: name ;
		nextPutAll: ' => ';
		print: variableClassName;
		nextPutAll: ')'
]

{ #category : #accessing }
CDVariableNode >> sourceInterval [
	^self start to: self stop
]

{ #category : #selection }
CDVariableNode >> start [

	^ start
]

{ #category : #accessing }
CDVariableNode >> start: anInteger [
	start := anInteger
]

{ #category : #selection }
CDVariableNode >> stop [
	^ stop
]

{ #category : #accessing }
CDVariableNode >> stop: anInteger [

	stop := anInteger
]

{ #category : #accessing }
CDVariableNode >> variableClassName [
	"Return a symbol representing the class of the variable i.e., 'x' => InstanceVariableSlot slotClassName returns
	#InstanceVariableSlot"

	^ variableClassName
]

{ #category : #accessing }
CDVariableNode >> variableClassName: aSymbol [

	variableClassName := aSymbol
]
