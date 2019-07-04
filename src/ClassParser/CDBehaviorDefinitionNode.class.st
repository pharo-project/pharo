"
I contain the methods shared between class and meta class definition.
"
Class {
	#name : #CDBehaviorDefinitionNode,
	#superclass : #CDNode,
	#instVars : [
		'classNameNode',
		'slotNodes',
		'traitDefinition',
		'className',
		'classKind'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDBehaviorDefinitionNode >> addSlot: aCDSlotNode [
	
	slotNodes add: aCDSlotNode.
	self addChild: aCDSlotNode
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> classDefinitionNode [
	^self
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> classKind: aString [ 
	classKind := aString
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> className [

	^ className
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> className: aClassName [
	className := aClassName
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> className: aName astNode: astNode [

	className := aName.
	classNameNode := astNode.
	self addChild: astNode
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> classNameNode [
	
	^ classNameNode
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> classNameNode: aClassNameNode [
	classNameNode := aClassNameNode
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> classNameNode: aNode astNode: astNode [

	classNameNode := aNode.
	self addChild: aNode
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> existingClassIfAbsent: aBlock [

	^self classNameNode existingClassIfAbsent: aBlock
]

{ #category : #initialization }
CDBehaviorDefinitionNode >> initialize [
	super initialize.
	slotNodes := OrderedCollection new
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isBytes [
	
	^ classKind = #variableByteSubclass:
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isEphemeron [
	
	^ classKind = #ephemeronSubclass:
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isImmediate [
	
	^ classKind = #immediateSubclass:
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isNormal [
	
	^ classKind = #subclass:
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isVariableClass [
	
	^ classKind = #variableSubclass:
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isWeak [
	
	^ classKind = #weakSubclass:
]

{ #category : #testing }
CDBehaviorDefinitionNode >> isWords [
	
	^ classKind = #variableWordSubclass:
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> slotNodes [
	
	^ slotNodes
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> slots [

	^ slotNodes
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> slots: aCollection [ 
	
	slotNodes := aCollection.
	aCollection do: [ :each | self addChild: each ]
	
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> traitDefinition [

	^traitDefinition
]

{ #category : #accessing }
CDBehaviorDefinitionNode >> traitDefinition: aNode [

	traitDefinition := aNode.
	self addChild: aNode.
]
