"
I'm the common superclass of all the class definition nodes.
A class definition is an object that is built out of the message sent to a class by the class definition parser.

I keep a reference to the original method node (originalNode) I'm created from.

Please note the vocabulary, we use the term class definition and class definition parser to refer to the class definition only and not the class with its methods.
"
Class {
	#name : 'CDNode',
	#superclass : 'Object',
	#instVars : [
		'parent',
		'originalNode',
		'children'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'testing' }
CDNode class >> isAbstract [

	^ self == CDNode
]

{ #category : 'instance creation' }
CDNode class >> on: aNode [

	^ self new
		originalNode: aNode;
		yourself
]

{ #category : 'accessing' }
CDNode >> addChild: aChild [

	children add: aChild.
	aChild parent: self
]

{ #category : 'compatibility' }
CDNode >> ast [

	^ self
]

{ #category : 'selection' }
CDNode >> bestNodeFor: anInterval [
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	selectedChildren := self children
				select: [:each | each intersectsInterval: anInterval].
	^selectedChildren size == 1
		ifTrue: [selectedChildren first bestNodeFor: anInterval]
		ifFalse: [self]
]

{ #category : 'selection' }
CDNode >> children [

	^ children
]

{ #category : 'accessing' }
CDNode >> classDefinitionNode [
	^parent classDefinitionNode
]

{ #category : 'selection' }
CDNode >> containedBy: anInterval [
	^anInterval first <= self start and: [anInterval last >= self stop]
]

{ #category : 'initialization' }
CDNode >> initialize [

	super initialize.
	children := OrderedCollection new
]

{ #category : 'selection' }
CDNode >> intersectsInterval: anInterval [
	^(anInterval first between: self start and: self stop)
		or: [self start between: anInterval first and: anInterval last]
]

{ #category : 'testing' }
CDNode >> isMessage [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : 'testing' }
CDNode >> isMethod [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : 'testing' }
CDNode >> isVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : 'accessing' }
CDNode >> originalNode [

	^originalNode
]

{ #category : 'accessing' }
CDNode >> originalNode: aNode [

	originalNode := aNode
]

{ #category : 'accessing' }
CDNode >> originalNodeSource [

	^originalNode source
]

{ #category : 'accessing' }
CDNode >> parent [
	^ parent
]

{ #category : 'accessing' }
CDNode >> parent: anObject [
	parent := anObject
]

{ #category : 'accessing' }
CDNode >> sourceInterval [
	^originalNode sourceInterval
]

{ #category : 'selection' }
CDNode >> start [

	^ originalNode start
]

{ #category : 'selection' }
CDNode >> stop [

	^ originalNode stop
]
