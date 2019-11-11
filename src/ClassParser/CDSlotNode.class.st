Class {
	#name : #CDSlotNode,
	#superclass : #CDNode,
	#instVars : [
		'node',
		'index',
		'start',
		'stop',
		'name',
		'initializationMessage',
		'slotClassName'
	],
	#category : #'ClassParser-Model'
}

{ #category : #'instance creation' }
CDSlotNode class >> node: aNode name: aName slotClassName: aSymbol initializationMessage: aMessageNode start: start stop: stop [
	
	^ self new
		node: aNode;
		name: aName;
		slotClassName: aSymbol;
		initializationMessage: aMessageNode;
		start: start;
		stop: stop;
		yourself
]

{ #category : #'instance creation' }
CDSlotNode class >> node: aNode name: aName slotClassName: aSymbol start: start stop: stop [
	
	^ self new
		node: aNode;
		name: aName;
		slotClassName: aSymbol;
		start: start;
		stop: stop;
		yourself
]

{ #category : #transforming }
CDSlotNode >> asSlot [
	^self name asSlot
]

{ #category : #accessing }
CDSlotNode >> binding [
	"To be polymorphic to RB method nodes"
	^self
]

{ #category : #accessing }
CDSlotNode >> index: anInteger [ 
	
	index := anInteger
]

{ #category : #accessing }
CDSlotNode >> initializationMessage [
	"Return the message node representing the initialization part of a slot."
	
	^ initializationMessage 
]

{ #category : #accessing }
CDSlotNode >> initializationMessage: aMessageNode [ 
	
	initializationMessage := aMessageNode
]

{ #category : #testing }
CDSlotNode >> isClassVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSlotNode >> isGlobalVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSlotNode >> isInstance [
	"To be polymorphic to RB method nodes"
	^true
]

{ #category : #testing }
CDSlotNode >> isLiteralVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSlotNode >> isTemp [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSlotNode >> isUndeclared [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSlotNode >> isVariable [
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

{ #category : #accessing }
CDSlotNode >> name [

	^ name
]

{ #category : #accessing }
CDSlotNode >> name: aString [

	name := aString
]

{ #category : #accessing }
CDSlotNode >> node: aNode [ 
	node := aNode
]

{ #category : #printing }
CDSlotNode >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: name ;
		nextPutAll: ' => ';
		print: slotClassName;
		nextPutAll: ')'
]

{ #category : #accessing }
CDSlotNode >> slotClassName [
	"Return a symbol representing the class of the slot: i.e., 'x' => InstanceVariableSlot slotClassName returns 
	#InstanceVariableSlot"
	
	^ slotClassName
]

{ #category : #accessing }
CDSlotNode >> slotClassName: aSymbol [ 
	
	slotClassName := aSymbol
]

{ #category : #accessing }
CDSlotNode >> sourceInterval [
	^start to: stop
]

{ #category : #selection }
CDSlotNode >> start [

	^ start
]

{ #category : #accessing }
CDSlotNode >> start: anInteger [ 
	start := anInteger
]

{ #category : #selection }
CDSlotNode >> stop [
	^ stop
]

{ #category : #accessing }
CDSlotNode >> stop: anInteger [ 
	
	stop := anInteger
]
