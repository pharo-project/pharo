Class {
	#name : #CDSlotNode,
	#superclass : #CDNode,
	#instVars : [
		'slot',
		'node',
		'index',
		'start',
		'stop',
		'name',
		'typeName',
		'initializationMessage'
	],
	#category : #'ClassParser-Model'
}

{ #category : #'instance-creation' }
CDSlotNode class >> node: aNode name: aName typeName: aTypeName initializationMessage: aString start: start stop: stop [
	
	^ self new
		node: aNode;
		name: aName;
		typeName: aTypeName;
		initializationMessage: aString;
		start: start;
		stop: stop;
		yourself
]

{ #category : #'instance-creation' }
CDSlotNode class >> node: aNode name: aName typeName: aTypeName start: start stop: stop [
	
	^ self new
		node: aNode;
		name: aName;
		typeName: aTypeName;
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
	
	^ initializationMessage 
]

{ #category : #accessing }
CDSlotNode >> initializationMessage: aString [ 
	
	initializationMessage := aString
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
		print: typeName;
		nextPutAll: ')'
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

{ #category : #accessing }
CDSlotNode >> typeName [
	"Return the type of the slot: i.e., 'x' => InstanceVariableSlot typeName will return 
	#InstanceVariableSlot"
	
	^ typeName
]

{ #category : #accessing }
CDSlotNode >> typeName: aString [ 
	
	typeName := aString
]
