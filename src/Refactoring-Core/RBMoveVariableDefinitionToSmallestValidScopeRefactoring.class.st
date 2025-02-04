"
I am a refactoring for moving the definition of a variable to the block/scope where it is used.

For a method temporary variable declared but not initialized in the method scope and only used within a block, the definition can be moved to the block using this variable.
"
Class {
	#name : 'RBMoveVariableDefinitionToSmallestValidScopeRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'interval',
		'name',
		'parseTree',
		'blockNodes',
		'definingNode'
	],
	#category : 'Refactoring-Core-Refactorings-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings-Unused'
}

{ #category : 'instance creation' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring class >> bindTight: anInterval in: aClass selector: aSelector [
	^ self new
		class: aClass
		selector: aSelector
		interval: anInterval
]

{ #category : 'instance creation' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring class >> model: aRBSmalltalk bindTight: anInterval in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			interval: anInterval;
		yourself
]

{ #category : 'preconditions' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: selector in: class).
		  (RBCondition withBlock: [
			   | methodSource |
			   interval first <= interval last ifFalse: [
				   self refactoringError: 'You must select a variable name' ].
			   methodSource := class sourceCodeFor: selector.
			   methodSource size >= interval last ifFalse: [
				   self refactoringError: 'Invalid range for variable' ].
			   name := methodSource copyFrom: interval first to: interval last.
			   (self checkInstanceVariableName: name in: class) ifFalse: [
				   self refactoringError:
					   name , ' does not seem to be a valid variable name.' ].
			   parseTree := class parseTreeForSelector: selector.
			   self checkParseTree.
			   true ]) }
]

{ #category : 'transforming' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> checkNodes: sequenceNodes [
	(sequenceNodes
		anySatisfy: [:each | OCReadBeforeWrittenTester isVariable: name readBeforeWrittenIn: each])
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

{ #category : 'transforming' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> checkParseTree [

	| node |

	blockNodes := OrderedCollection new.
	node := self whichVariableNode: parseTree inInterval: interval name: name.
	node ifNil: [ self refactoringError: 'Unable to locate node in parse tree' ].
	definingNode := node whoDefines: name.
	definingNode ifNil: [ self refactoringError: 'Cannot locate variable definition' ].
	definingNode isSequence
		ifFalse: [ self refactoringError: 'Variable is an argument' ].
	( self usesDirectly: definingNode )
		ifTrue: [ self refactoringError: 'Variable already bound tightly as possible' ].
	( self checkNodes: ( self subblocksIn: definingNode ) )
		ifFalse: [ self refactoringError: 'Variable is possibly read before written' ]
]

{ #category : 'scripting api - conditions' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'initialization' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> class: aClass selector: aSelector interval: anInterval [
	interval := anInterval.
	class := self classObjectFor: aClass.
	selector := aSelector
]

{ #category : 'transforming' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> privateTransform [
	definingNode removeTemporaryNamed: name.
	blockNodes do: [:each | each body addTemporaryNamed: name].
	class compileTree: parseTree
]

{ #category : 'storing' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> storeOn: aStream [
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

{ #category : 'transforming' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> subblocksIn: aParseTree [
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

{ #category : 'transforming' }
RBMoveVariableDefinitionToSmallestValidScopeRefactoring >> usesDirectly: aParseTree [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '[:`@args | | `@temps | `@.Statements]'
			do: [ :aNode :answer | answer ];
		matches: name do: [ :aNode :answer | true ].
	^ searcher executeTree: aParseTree initialAnswer: false
]
