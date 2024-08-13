Class {
	#name : 'CDClassDefinitionNode',
	#superclass : 'CDBehaviorDefinitionNode',
	#instVars : [
		'superclassNameNode',
		'packageNameNode',
		'sharedPools',
		'superclassName',
		'tag',
		'sharedVariableNodes'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'adding' }
CDClassDefinitionNode >> addSharedVariable: aCDSharedVariableNode [

	self sharedVariables add: aCDSharedVariableNode.
	self addChild: aCDSharedVariableNode
]

{ #category : 'accessing' }
CDClassDefinitionNode >> binding [
	^ self class environment associationAt: self className ifAbsent: [LiteralVariable key: nil value: self]
]

{ #category : 'initialization' }
CDClassDefinitionNode >> initialize [

	super initialize.
	sharedVariableNodes := OrderedCollection new
]

{ #category : 'testing' }
CDClassDefinitionNode >> isClassDefinition [

	^ true
]

{ #category : 'testing' }
CDClassDefinitionNode >> isClassSide [
	^ false
]

{ #category : 'testing' }
CDClassDefinitionNode >> isInstanceSide [
	^ true
]

{ #category : 'accessing' }
CDClassDefinitionNode >> packageName [

	^ packageNameNode ifNotNil: [packageNameNode value asSymbol ]
]

{ #category : 'accessing' }
CDClassDefinitionNode >> packageNameNode [

	^ packageNameNode
]

{ #category : 'accessing' }
CDClassDefinitionNode >> packageNameNode: aNode astNode: astNode [

	packageNameNode := (CDPackageNode on: aNode) packageName: aNode value.
	self addChild: aNode
]

{ #category : 'accessing' }
CDClassDefinitionNode >> sharedPools [

	^ sharedPools ifNil: [ sharedPools := OrderedCollection new ]
]

{ #category : 'accessing' }
CDClassDefinitionNode >> sharedPools: aCollection [
	sharedPools := aCollection.
	aCollection do: [ :each | self addChild: each ]
]

{ #category : 'accessing' }
CDClassDefinitionNode >> sharedVariableNodes [

	^ sharedVariableNodes
]

{ #category : 'accessing' }
CDClassDefinitionNode >> sharedVariables [

	^ sharedVariableNodes
]

{ #category : 'accessing' }
CDClassDefinitionNode >> sharedVariables: aCollection [
	sharedVariableNodes := aCollection.
	aCollection do: [ :each | self addChild: each ]
]

{ #category : 'accessing' }
CDClassDefinitionNode >> superclassName [

	^ superclassName
]

{ #category : 'accessing' }
CDClassDefinitionNode >> superclassName: aString [
	superclassName := aString
]

{ #category : 'accessing' }
CDClassDefinitionNode >> superclassName: aName astNode: astNode [

	superclassName := aName.
	superclassNameNode := astNode.
	self addChild: astNode
]

{ #category : 'accessing' }
CDClassDefinitionNode >> superclassNameNode [

	^ superclassNameNode
]

{ #category : 'accessing' }
CDClassDefinitionNode >> superclassNameNode: anObject [
	superclassNameNode := anObject
]

{ #category : 'accessing' }
CDClassDefinitionNode >> tag [

	^ tag
]

{ #category : 'accessing' }
CDClassDefinitionNode >> tag: aString [

	tag := aString
]
