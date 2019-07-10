"
Moves the definition of a temporary variable to the block/scope where it is used.

For a temporary variable defined in a method but only initialized and used within a block, the transformation moves the definition to the block which uses this variable.

The transformation automatically searches for the block(s) where the definition could be moved to.

Usage:
| transformation |
transformation := (RBMoveTemporaryVariableDefinitionTransformation
				variable: #temp
				inMethod: #moveDefinition
				inClass: #RBDummyRefactoryTestDataApp)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- there is a block where the variable definition can be moved to.
"
Class {
	#name : #RBMoveTemporaryVariableDefinitionTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		'variableName',
		'blockNodes'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBMoveTemporaryVariableDefinitionTransformation class >> model: aRBModel variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		model: aRBModel;
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : #api }
RBMoveTemporaryVariableDefinitionTransformation class >> variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : #converting }
RBMoveTemporaryVariableDefinitionTransformation >> asRefactoring [

	^ super asRefactoring
		delegatesPreconditions: false;
		yourself
]

{ #category : #executing }
RBMoveTemporaryVariableDefinitionTransformation >> buildTransformations [

	"ifnil... tight blocks should not be returned as errors"
	blockNodes ifNil: [ self checkMethodForBlocks ].
	
	^ (OrderedCollection
		withAll: ((blockNodes
			sorted: [ :a :b | a start > b start ])
			collect: [ :blockNode | 
				RBAddTemporaryVariableTransformation
					variable: variableName
					inInterval: blockNode body sourceInterval
					inMethod: selector
					inClass: class ]))
		add: (RBRemoveTemporaryVariableTransformation
					variable: variableName
					inMethod: selector
					inClass: class);
		yourself
]

{ #category : #private }
RBMoveTemporaryVariableDefinitionTransformation >> checkAllBlocksIn: aParseTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '[:`@blockTemps | | `@temps | `@.Statements]'
		do: [ :aNode :answer | 
			(aNode references: variableName)
				ifTrue: [ answer add: aNode ].
			answer ].
	^ searcher
		executeTree: aParseTree
		initialAnswer: OrderedCollection new
]

{ #category : #private }
RBMoveTemporaryVariableDefinitionTransformation >> checkBlocksIn: aParseTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '[:`@args | | `@temps | `@.Statements]'
			do: [ :aNode :answer | answer ];
		matches: variableName do: [ :aNode :answer | true ].
	^ searcher executeTree: aParseTree initialAnswer: false
]

{ #category : #private }
RBMoveTemporaryVariableDefinitionTransformation >> checkLocationsIn: candidateBlocks [

	(candidateBlocks 
		detect: [:each | RBReadBeforeWrittenTester
							isVariable: variableName readBeforeWrittenIn: each]
		ifNone: [nil]) notNil 
		ifTrue: [^false].
		
	blockNodes ifNil: [ blockNodes := OrderedCollection new ].
	candidateBlocks do: [ :each | 
		(self checkBlocksIn: each body) 
			ifTrue: [ blockNodes add: each ]
			ifFalse: 	[ (self checkLocationsIn: (self checkAllBlocksIn: each body)) 
							ifFalse: [ blockNodes add: each ]]].
	^ true
]

{ #category : #private }
RBMoveTemporaryVariableDefinitionTransformation >> checkMethodForBlocks [

	| definingNode |
	definingNode := self definingBlock.
	self checkBlocksIn: definingNode.
	self checkLocationsIn: (self checkAllBlocksIn: definingNode)
]

{ #category : #private }
RBMoveTemporaryVariableDefinitionTransformation >> definingBlock [

	| node definingNode |
	node := self definingMethod
		ifNil: [ self refactoringError: 'Method does not exist' ]
		ifNotNil: [ :methodNode | methodNode variableWith: variableName ].
	node ifNil: [self refactoringError: 'Unable to locate temporary variable in parse tree'].
	
	definingNode := node whoDefines: variableName.
	definingNode 
		ifNil: [self refactoringError: 'Cannot locate variable definition'].
	definingNode isSequence
		ifFalse: [self refactoringError: 'Variable is an argument'].
		
	^ definingNode
]

{ #category : #preconditions }
RBMoveTemporaryVariableDefinitionTransformation >> preconditions [

	^ ( RBCondition
			withBlock: [ self definingClass isNotNil ]
			errorString: 'No such class or trait named ', class asString)
		& ( RBCondition definesSelector: selector in: self definingClass)
		& ( RBCondition
			withBlock: [ self definingMethod allTemporaryVariables includes: variableName ]
			errorString: 'Method named ', selector, ' does not define a temporary variable named ', variableName )
		& ( RBCondition
			withBlock: [ (self definingMethod allArgumentVariables includes: variableName) not ]
			errorString: 'Variable named ', variableName, ' cannot be removed because it is an argument in this method' )
		& ( RBCondition
			withBlock: [ (self checkBlocksIn: self definingBlock) not ]
			errorString: 'Variable named ', variableName, ' is already bound tightly as possible.')
		& ( RBCondition
			withBlock: [ self checkLocationsIn: (self checkAllBlocksIn: self definingBlock) ]
			errorString: 'Variable named ', variableName, ' is used in an outside block.')
]

{ #category : #printing }
RBMoveTemporaryVariableDefinitionTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' variable: ''';
		nextPutAll: variableName;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #api }
RBMoveTemporaryVariableDefinitionTransformation >> variable: aVariableName inMethod: aSelector inClass: aClass [

	class := aClass.
	selector := aSelector.
	variableName := aVariableName
]
