"
I am a refactoring for moving the definition of a variable to the block/scope where it is used.

For a method temporary variable declared but not initialized in the method scope and only used within a block, the definition can be moved to the block using this variable.
"
Class {
	#name : #RBMoveVariableDefinitionRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'interval',
		'name',
		'parseTree',
		'blockNodes',
		'definingNode'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBMoveVariableDefinitionRefactoring class >> bindTight: anInterval in: aClass selector: aSelector [ 
	^self new
		class: aClass
		selector: aSelector
		interval: anInterval
]

{ #category : #'instance creation' }
RBMoveVariableDefinitionRefactoring class >> model: aRBSmalltalk bindTight: anInterval in: aClass selector: aSelector [ 
	^(self new)
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			interval: anInterval;
		yourself
]

{ #category : #transforming }
RBMoveVariableDefinitionRefactoring >> checkNodes: sequenceNodes [ 
	(sequenceNodes 
		anySatisfy: [:each | RBReadBeforeWrittenTester isVariable: name readBeforeWrittenIn: each]) 
		ifTrue: [^false].
	sequenceNodes do: 
			[:each | 
			(self usesDirectly: each body) 
				ifTrue: [blockNodes add: each]
				ifFalse: 
					[(self checkNodes: (self subblocksIn: each body)) 
						ifFalse: [blockNodes add: each]]].
	^true
]

{ #category : #transforming }
RBMoveVariableDefinitionRefactoring >> checkParseTree [

	| node |

	blockNodes := OrderedCollection new.
	node := self whichVariableNode: parseTree inInterval: interval name: name.
	node ifNil: [ self refactoringFailure: 'Unable to locate node in parse tree' ].
	definingNode := node whoDefines: name.
	definingNode ifNil: [ self refactoringFailure: 'Cannot locate variable definition' ].
	definingNode isSequence
		ifFalse: [ self refactoringFailure: 'Variable is an argument' ].
	( self usesDirectly: definingNode )
		ifTrue: [ self refactoringFailure: 'Variable already bound tightly as possible' ].
	( self checkNodes: ( self subblocksIn: definingNode ) )
		ifFalse: [ self refactoringError: 'Variable is possibly read before written' ]
]

{ #category : #initialization }
RBMoveVariableDefinitionRefactoring >> class: aClass selector: aSelector interval: anInterval [ 
	interval := anInterval.
	class := self classObjectFor: aClass.
	selector := aSelector
]

{ #category : #preconditions }
RBMoveVariableDefinitionRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[| methodSource |
					interval first <= interval last 
						ifFalse: [self refactoringFailure: 'You must select a variable name'].
					methodSource := class sourceCodeFor: selector.
					methodSource size >= interval last 
						ifFalse: [self refactoringFailure: 'Invalid range for variable'].
					name := methodSource copyFrom: interval first to: interval last.
					(self checkInstanceVariableName: name in: class) 
						ifFalse: 
							[self 
								refactoringFailure: name , ' does not seem to be a valid variable name.'].
					parseTree := class parseTreeFor: selector.
					self checkParseTree.
					true])
]

{ #category : #printing }
RBMoveVariableDefinitionRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' bindTight: '.
	interval storeOn: aStream.
	aStream nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector.
	aStream nextPut: $)
]

{ #category : #transforming }
RBMoveVariableDefinitionRefactoring >> subblocksIn: aParseTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '[:`@blockTemps | | `@temps | `@.Statements]'
		do: [ :aNode :answer | 
			(aNode references: name)
				ifTrue: [ answer add: aNode ].
			answer ].
	^ searcher
		executeTree: aParseTree
		initialAnswer: OrderedCollection new
]

{ #category : #transforming }
RBMoveVariableDefinitionRefactoring >> transform [
	definingNode removeTemporaryNamed: name.
	blockNodes do: [:each | each body addTemporaryNamed: name].
	class compileTree: parseTree
]

{ #category : #transforming }
RBMoveVariableDefinitionRefactoring >> usesDirectly: aParseTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '[:`@args | | `@temps | `@.Statements]'
			do: [ :aNode :answer | answer ];
		matches: name do: [ :aNode :answer | true ].
	^ searcher executeTree: aParseTree initialAnswer: false
]
