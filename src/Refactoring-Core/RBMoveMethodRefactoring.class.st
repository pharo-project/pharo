"
I am a refactoring for moving a method from the class to one of its instance variable objects.

Moving a method moves it implementation to one or more classes and replaces the implementation in the original method by a delegation to one of the classes instance variable. 

I expect an option for selecting the type (classes) to which this method should be added.
A role typer RBRefactoryTyper is used to guess the possible classes used for this instance variables.
And an option for requesting the new method selector.

For all selected classes a method implementing the original method is created, and if the original code uses some references to self, a parameter needs to be added to provided the former implementor.

For example, moving the method #isBlack from class Color to its instvar #rgb for the type ""Integer"" creates a method 
Integer>>#isBlack
 ^ self = 0

and changes Colors implementation from: 
Color>>#isBlack
   ^ rgb = 0
to: 
Color>>#isBlack
   ^ rgb isBlack

"
Class {
	#name : #RBMoveMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'variable',
		'moveToClasses',
		'parseTree',
		'hasOnlySelfReturns',
		'selfVariableName'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBMoveMethodRefactoring class >> model: aRBSmalltalk selector: aSymbol class: aClass variable: aVariableName [
	^ self new
		model: aRBSmalltalk;
		selector: aSymbol class: aClass variable: aVariableName;
		yourself
]

{ #category : #'instance creation' }
RBMoveMethodRefactoring class >> selector: aSymbol class: aClass variable: aVariableName [
	^ self new
		selector: aSymbol class: aClass variable: aVariableName;
		yourself
]

{ #category : #transforming }
RBMoveMethodRefactoring >> abstractVariables [
	self performComponentRefactoring: self abstractVariablesRefactoring.
	parseTree := self abstractVariablesRefactoring parseTree
]

{ #category : #transforming }
RBMoveMethodRefactoring >> abstractVariablesRefactoring [
	^RBAbstractVariablesRefactoring 
		model: self model
		abstractVariablesIn: parseTree
		from: class
		toAll: moveToClasses
		ignoring: variable
]

{ #category : #transforming }
RBMoveMethodRefactoring >> addSelfReturn [
	self hasOnlySelfReturns ifTrue: [^self].
	parseTree addSelfReturn
]

{ #category : #private }
RBMoveMethodRefactoring >> buildParseTree [

	parseTree := ( class parseTreeFor: selector ) copy.
	parseTree ifNil: [ self refactoringFailure: 'Could not parse method' ]
]

{ #category : #preconditions }
RBMoveMethodRefactoring >> checkAssignmentsToVariable [
	| searcher |
	variable
		ifNotNil: [ searcher := self parseTreeSearcher.
			searcher
				matches: variable , ' := `@object'
				do: [ :aNode :answer | true ].
			(searcher executeTree: parseTree initialAnswer: false)
				ifTrue: [ self
						refactoringError:
							('Cannot move the method into <1s> since it is assigned'
								expandMacrosWith: variable) ] ]
]

{ #category : #preconditions }
RBMoveMethodRefactoring >> checkForPrimitiveMethod [
	parseTree isPrimitive 
		ifTrue: [self refactoringError: 'Cannot move primitive methods']
]

{ #category : #preconditions }
RBMoveMethodRefactoring >> checkForSuperReferences [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: 'super `@message: `@args'
		do: [ :aNode :answer | true ].
	(searcher executeTree: parseTree initialAnswer: false)
		ifTrue: [ self
				refactoringError: 'Cannot move the method since it has a super message send.' ]
]

{ #category : #preconditions }
RBMoveMethodRefactoring >> checkTemporaryVariableNames [
	| varNames |
	varNames := parseTree allDefinedVariables.
	selfVariableName ifNotNil: [varNames add: selfVariableName].
	varNames do: 
			[:name | 
			moveToClasses do: 
					[:each | 
					(self canReferenceVariable: name in: each) 
						ifTrue: 
							[self refactoringError: ('<1p> already defines a variable called <2s>' 
										expandMacrosWith: each
										with: name)]]]
]

{ #category : #transforming }
RBMoveMethodRefactoring >> compileDelegatorMethod [
	| statementNode delegatorNode tree |
	delegatorNode := RBMessageNode 
				receiver: (RBVariableNode named: variable)
				selector: parseTree selector
				keywordsPositions: parseTree keywordsPositions
				arguments: (parseTree argumentNames collect: 
							[:each | 
							RBVariableNode 
								named: (each = selfVariableName ifTrue: ['self'] ifFalse: [each])]).
	self hasOnlySelfReturns 
		ifFalse: [delegatorNode := RBReturnNode value: delegatorNode].
	statementNode := RBSequenceNode temporaries: #()
				statements: (Array with: delegatorNode).
	(tree := class parseTreeFor: selector) body: statementNode.
	class compileTree: tree
]

{ #category : #transforming }
RBMoveMethodRefactoring >> compileNewMethods [
	moveToClasses 
		do: [:each | each compile: parseTree newSource withAttributesFrom: (class methodFor: selector)]
]

{ #category : #transforming }
RBMoveMethodRefactoring >> getArgumentNameForSelf [
	self needsToReplaceSelfReferences ifFalse: [^self].
	
	[selfVariableName := self requestSelfArgumentName.
	(self checkInstanceVariableName: selfVariableName in: class) 
		ifTrue: 
			[self verifyTemporaryVariableDoesNotOverride 
				ifFalse: 
					[self 
						refactoringWarning: 'The variable is already defined in one of the classes you''re moving the method to.<n>Try another?' 
								expandMacros.
					selfVariableName := nil]]
		ifFalse: 
			[self 
				refactoringWarning: 'The variable name is not a valid Smalltalk temporary variable name<n>Try again?' 
						expandMacros.
			selfVariableName := nil].
	selfVariableName isNil] 
			whileTrue: []
]

{ #category : #'private-accessing' }
RBMoveMethodRefactoring >> getClassForGlobalOrClassVariable [
	| definingClass type |
	definingClass := class whoDefinesClassVariable: (variable ifNil: ['']).
	definingClass ifNil: [ 
			type := self model classNamed: variable.
			type ifNil: [ type := self model classNamed: #Object ] ]
		ifNotNil: [ type := definingClass typeOfClassVariable: variable ].
	moveToClasses := self selectVariableTypesFrom: (Array with: type) selected: (Array with: type).
	moveToClasses ifNil: [ self refactoringFailure: 'Method not moved' ]
]

{ #category : #'private-accessing' }
RBMoveMethodRefactoring >> getClassesForInstanceVariable [

	| definingClass typer types |

	definingClass := class whoDefinesInstanceVariable: variable.
	typer := RBRefactoryTyper newFor: self model.
	typer runOn: definingClass.
	types := typer typesFor: variable.
	types ifEmpty: [ types := OrderedCollection with: ( self model classNamed: #Object ) ].
	moveToClasses := self selectVariableTypesFrom: types selected: ( typer guessTypesFor: variable ).
	moveToClasses ifNil: [ self refactoringFailure: 'Method not moved' ]
]

{ #category : #'private-accessing' }
RBMoveMethodRefactoring >> getClassesForTemporaryVariable [

	| types |

	types := RBRefactoryTyper typesFor: variable in: parseTree model: self model.
	types ifEmpty: [ types := OrderedCollection with: ( self model classNamed: #Object ) ].
	moveToClasses := self selectVariableTypesFrom: types selected: types.
	moveToClasses ifNil: [ self refactoringFailure: 'Method not moved' ]
]

{ #category : #'private-accessing' }
RBMoveMethodRefactoring >> getClassesToMoveTo [
	self isMovingToArgument 
		ifTrue: [self getClassesForTemporaryVariable]
		ifFalse: 
			[self isMovingToInstVar 
				ifTrue: [self getClassesForInstanceVariable]
				ifFalse: [self getClassForGlobalOrClassVariable]].
	moveToClasses ifEmpty: [self refactoringFailure: 'No classes selected, method not moved.']
]

{ #category : #preconditions }
RBMoveMethodRefactoring >> getNewMethodName [

	| newSelector parameters alreadyDefined methodName newMethodName |

	self removeArgument.
	parameters := parseTree argumentNames asOrderedCollection.	"parameters remove: variable ifAbsent: []."
	self needsToReplaceSelfReferences
		ifTrue: [ parameters add: selfVariableName ].
	methodName := RBMethodName
		selector: ( self uniqueMethodNameFor: parameters size )
		arguments: parameters.

	[ newMethodName := self requestMethodNameFor: methodName.
	newMethodName ifNil: [ self refactoringFailure: 'Did not move method' ].
	newMethodName isValid
		ifTrue: [ newSelector := newMethodName selector ]
		ifFalse: [ self refactoringWarning: 'Invalid method name' ].
	parameters := newMethodName arguments.
	( self checkMethodName: newSelector in: class )
		ifFalse: [ self refactoringWarning: newSelector , ' is not a valid selector name.'.
			newSelector := nil
			].
	alreadyDefined := moveToClasses
		detect: [ :each | each hierarchyDefinesMethod: newSelector ]
		ifNone: [ nil ].
	alreadyDefined ifNotNil: [ self
				refactoringWarning:
					( '<1s> is already defined by <2p> or a super/subclass<n>Try another?'
						expandMacrosWith: newSelector
						with: alreadyDefined ).
			newSelector := nil
			].
	newSelector isNil
	] whileTrue: [  ].
	parseTree
		renameSelector: newSelector
		andArguments: ( parameters collect: [ :each | RBVariableNode named: each ] ) asArray
]

{ #category : #transforming }
RBMoveMethodRefactoring >> hasOnlySelfReturns [

	^ hasOnlySelfReturns
		ifNil: [ | searcher |

			searcher := self parseTreeSearcher.
			searcher
				matches: '^self' do: [ :aNode :answer | answer ];
				matches: '^`@object' do: [ :aNode :answer | false ].
			hasOnlySelfReturns := searcher executeTree: parseTree initialAnswer: true
			]
		ifNotNil: [ hasOnlySelfReturns ]
]

{ #category : #testing }
RBMoveMethodRefactoring >> hasSelfReferences [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher matches: 'self' do: [ :aNode :answer | true ].
	self hasOnlySelfReturns
		ifTrue: [ searcher matches: '^self' do: [ :aNode :answer | answer ] ].
	^ searcher executeTree: parseTree initialAnswer: false
]

{ #category : #testing }
RBMoveMethodRefactoring >> isMovingToArgument [
	^(parseTree arguments collect: [:each | each name]) includes: variable
]

{ #category : #testing }
RBMoveMethodRefactoring >> isMovingToInstVar [
	^self isMovingToArgument not 
		and: [(class whoDefinesInstanceVariable: variable) notNil]
]

{ #category : #testing }
RBMoveMethodRefactoring >> needsToReplaceSelfReferences [
	^self hasSelfReferences 
		or: [self abstractVariablesRefactoring hasVariablesToAbstract]
]

{ #category : #preconditions }
RBMoveMethodRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[self buildParseTree.
					self checkForPrimitiveMethod.
					self checkForSuperReferences.
					self checkAssignmentsToVariable.
					self getClassesToMoveTo.
					self getArgumentNameForSelf.
					self checkTemporaryVariableNames.
					self getNewMethodName.
					true])
]

{ #category : #transforming }
RBMoveMethodRefactoring >> removeArgument [
	"Removes the excess argument if any.
	This argument is the variable which is
	referenced by self in the classes the
	method is moved to. "
	| removeIndex |
	removeIndex := parseTree argumentNames indexOf: variable.
	removeIndex = 0 ifFalse: 
		[parseTree 
			selector: ('' join: ((parseTree selector keywords asOrderedCollection) 
									removeAt: removeIndex; yourself)) asSymbol
			keywordsPositions: ((parseTree keywordsPositions asOrderedCollection) 
									removeAt: removeIndex; yourself) asIntegerArray
			arguments: ((parseTree arguments asOrderedCollection)  
									removeAt: removeIndex; yourself) asArray].
]

{ #category : #transforming }
RBMoveMethodRefactoring >> replaceSelfReferences [
	| replacer |
	self needsToReplaceSelfReferences ifTrue: [
		replacer := RBParseTreeRewriter new.
		replacer replace: 'self' with: selfVariableName.
		self hasOnlySelfReturns ifTrue: 
			[replacer replace: '^self' with: '^self'].
		replacer executeTree: parseTree.
		parseTree := replacer tree].
]

{ #category : #transforming }
RBMoveMethodRefactoring >> replaceVariableReferences [
	| replacer |
	replacer := RBParseTreeRewriter new.
	replacer replace: variable with: 'self'.
	replacer executeTree: parseTree.
	parseTree := replacer tree
]

{ #category : #initialization }
RBMoveMethodRefactoring >> selector: aSymbol class: aClass variable: aVariableName [ 
	selector := aSymbol.
	class := self classObjectFor: aClass.
	variable := aVariableName
]

{ #category : #printing }
RBMoveMethodRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector;
		nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' variable: ''';
		nextPutAll: variable;
		nextPutAll: ''')'
]

{ #category : #transforming }
RBMoveMethodRefactoring >> transform [
	self
		abstractVariables;
		addSelfReturn;
		replaceSelfReferences;
		replaceVariableReferences;
		compileNewMethods;
		compileDelegatorMethod
]

{ #category : #transforming }
RBMoveMethodRefactoring >> verifyTemporaryVariableDoesNotOverride [
	(parseTree allDefinedVariables includes: selfVariableName)
		ifTrue: [ ^ false ].
	^ moveToClasses 
		noneSatisfy: [ :each | each definesVariable: selfVariableName ]
]
