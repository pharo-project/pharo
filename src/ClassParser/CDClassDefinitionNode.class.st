Class {
	#name : #CDClassDefinitionNode,
	#superclass : #CDBehaviorDefinitionNode,
	#instVars : [
		'sharedSlotNodes',
		'superclassNameNode',
		'packageNameNode',
		'sharedPools',
		'superclassName',
		'tag'
	],
	#category : #'ClassParser-Model'
}

{ #category : #'instance creation' }
CDClassDefinitionNode class >> on: aRBMessageNode [ 
	
	^ self new
		originalNode: aRBMessageNode;
		yourself
]

{ #category : #accessing }
CDClassDefinitionNode >> binding [ 
	self trace: '.'.
	^ self class environment associationAt: self className ifAbsent: [LiteralVariable key: nil value: self].
]

{ #category : #testing }
CDClassDefinitionNode >> isClassDefinition [
	
	^ true
]

{ #category : #testing }
CDClassDefinitionNode >> isClassSideDefinition [
	"Pay attention that this is really to reflect that we have two class definitions node classes.
	We named this method like that and not as isClassDefinition because it conflicts 
	with other method."
	
	^ false
]

{ #category : #testing }
CDClassDefinitionNode >> isInstanceSideDefinition [
	"Pay attention that this is really to reflect that we have two class definitions node classes."
	"We named this method like that because there is another method called isClassSideDefinition 
	that cannot be name isClassDefinition (because of conflicts with other method)."
	
	^ true
]

{ #category : #accessing }
CDClassDefinitionNode >> packageName [
	
	^ packageNameNode ifNotNil: [packageNameNode value asSymbol ]
]

{ #category : #accessing }
CDClassDefinitionNode >> packageNameNode [
	
	^ packageNameNode
]

{ #category : #accessing }
CDClassDefinitionNode >> packageNameNode: aNode astNode: astNode [

	packageNameNode := aNode.
	self addChild: aNode.
]

{ #category : #testing }
CDClassDefinitionNode >> selector [

	^ tokens at: 2
]

{ #category : #accessing }
CDClassDefinitionNode >> sharedPools [
	sharedPools ifNil: [ self sharedPools: OrderedCollection new. ].
	^sharedPools
]

{ #category : #accessing }
CDClassDefinitionNode >> sharedPools: aCollection [ 
	sharedPools := aCollection.
	aCollection do: [ :each | self addChild: each ]
]

{ #category : #accessing }
CDClassDefinitionNode >> sharedSlotNodes [
	
	^ sharedSlotNodes
]

{ #category : #accessing }
CDClassDefinitionNode >> sharedSlots [
	^ sharedSlotNodes ifNil:[ sharedSlotNodes := Array new ]
	
]

{ #category : #accessing }
CDClassDefinitionNode >> sharedSlots: aCollection [ 
	sharedSlotNodes := aCollection.
	aCollection do: [ :each | self addChild: each ]
]

{ #category : #accessing }
CDClassDefinitionNode >> superclassName [
	
	^ superclassName
]

{ #category : #accessing }
CDClassDefinitionNode >> superclassName: aString [
	superclassName := aString
]

{ #category : #accessing }
CDClassDefinitionNode >> superclassName: aName astNode: astNode [
	
	superclassName := aName.
	superclassNameNode := astNode.
	self addChild: astNode.
]

{ #category : #accessing }
CDClassDefinitionNode >> superclassNameNode [
	
	^ superclassNameNode
]

{ #category : #accessing }
CDClassDefinitionNode >> superclassNameNode: anObject [
	superclassNameNode := anObject
]

{ #category : #accessing }
CDClassDefinitionNode >> tag [

	^ tag 
]

{ #category : #accessing }
CDClassDefinitionNode >> tag: aString [

	tag := aString
]
