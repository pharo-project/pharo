Class {
	#name : 'RBInlineMethodTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'sourceInterval',
		'inlineParseTree',
		'sourceParseTree',
		'sourceSelector',
		'sourceMessage',
		'checkOverridden',
		'classOfTheMethodToInline'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'instance creation' }
RBInlineMethodTransformation class >> inline: anInterval inMethod: aSelector forClass: aClass [
	^ self new
		inline: anInterval
		inMethod: aSelector
		forClass: aClass
]

{ #category : 'instance creation' }
RBInlineMethodTransformation class >> model: aRBSmalltalk inline: anInterval inMethod: aSelector forClass: aClass [
	^ self new
		model: aRBSmalltalk;
		inline: anInterval
			inMethod: aSelector
			forClass: aClass;
		yourself
]

{ #category : 'adding' }
RBInlineMethodTransformation >> addSelfReturn [
	inlineParseTree addSelfReturn
]

{ #category : 'adding' }
RBInlineMethodTransformation >> addTemporary: sourceNode assignedTo: replacementNode [
	| newName |
	newName := self renameConflictingTemporary: sourceNode name.
	(inlineParseTree body)
		addTemporaryNamed: newName;
		addNodeFirst: (RBAssignmentNode variable: (RBVariableNode named: newName)
					value: replacementNode)
]

{ #category : 'preconditions' }
RBInlineMethodTransformation >> applicabilityPreconditions [

	^ (RBCondition definesSelector: sourceSelector in: class)
	  & (RBCondition withBlock: [
			   self findSelectedMessage.
			   self checkOverridden ifTrue: [
				   self isOverridden ifTrue: [
					   self refactoringWarning:
						   ('<1p>>><2s> is overriden. Do you want to inline it anyway?'
							    expandMacrosWith: self classOfTheMethodToInline
							    with: self inlineSelector) ] ].
			   self parseInlineMethod.
			   self isPrimitive ifTrue: [
				   self refactoringError: 'Cannot inline primitives' ].
			   self checkSuperMessages.
			   self rewriteInlinedTree.
			   (sourceMessage parent isReturn or: [
				    self hasMultipleReturns not ]) ifFalse: [
				   self refactoringError:
					   'Cannot inline method since it contains multiple returns that cannot be rewritten' ].
			   true ])
]

{ #category : 'adding' }
RBInlineMethodTransformation >> checkOverridden [
	^ checkOverridden ifNil: [ checkOverridden := true ]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> checkSuperMessages [

	self classOfTheMethodToInline = class
		ifTrue: [ ^ self ].
	self classOfTheMethodToInline superclass ifNil: [ ^ self ].
	inlineParseTree superMessages
		do: [ :each |
			( self classOfTheMethodToInline superclass whoDefinesMethod: each ) = ( class superclass whoDefinesMethod: each )
				ifFalse: [ self
						refactoringError:
							( 'Cannot inline method since it sends a super message <1s> that is overriden'
								expandMacrosWith: each )
					]
			]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> classOfTheMethodToInline [

	^ classOfTheMethodToInline
		  ifNil: [
			  classOfTheMethodToInline := (sourceMessage receiver name
			                               = 'super'
				                               ifTrue: [ class superclass ]
				                               ifFalse: [ class ])
				                              whichClassIncludesSelector:
				                              self inlineSelector ]
		  ifNotNil: [ classOfTheMethodToInline ]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> compileMethod [

	class compileTree: sourceParseTree
]

{ #category : 'adding' }
RBInlineMethodTransformation >> findSelectedMessage [

	sourceParseTree := class parseTreeForSelector: sourceSelector.
	sourceParseTree ifNil: [ self refactoringError: 'Could not parse sources' ].
	sourceMessage := sourceParseTree whichNodeIsContainedBy: sourceInterval.
	sourceMessage
		ifNil: [ self refactoringError: 'The selection doesn''t appear to be a message send' ].
	sourceMessage isCascade
		ifTrue: [ sourceMessage := sourceMessage messages last ].
	sourceMessage isMessage
		ifFalse: [ self refactoringError: 'The selection doesn''t appear to be a message send' ].
	(sourceMessage receiver isSelfVariable or: [ sourceMessage receiver isSuperVariable ])
		ifFalse: [ self refactoringError: 'Cannot inline non-self messages' ]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> hasMultipleReturns [
	"Do we have multiple returns? If the last statement isn't a return, then we have an implicit return of self."

	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '^``@object'
		do: [ :aNode :hasAReturn |
			hasAReturn
				ifTrue: [ ^ true ].
			true ].
	searcher
		executeTree: inlineParseTree
		initialAnswer: inlineParseTree lastIsReturn not.
	^ false
]

{ #category : 'adding' }
RBInlineMethodTransformation >> inline: anInterval inMethod: aSelector forClass: aClass [
	sourceSelector := aSelector.
	class := self model classObjectFor: aClass.
	sourceInterval := anInterval
]

{ #category : 'adding' }
RBInlineMethodTransformation >> inlineParseTree [
	^ inlineParseTree
]

{ #category : 'adding' }
RBInlineMethodTransformation >> inlineSelector [

	sourceMessage ifNil: [ self findSelectedMessage ].
	^ sourceMessage selector
]

{ #category : 'adding' }
RBInlineMethodTransformation >> inlineSourceReplacing: aParseTree [
	| statements nodeUnderSequence |
	statements := inlineParseTree body statements.
	(statements size > 1 and: [ aParseTree isEvaluatedFirst not ]) ifTrue: [
		self refactoringWarning:
			'To inline this method, we need to move some of its statements before the original message send.<n>This could change the order of execution, which can change the behavior.<n>Do you want to proceed?'
				expandMacros ].
	nodeUnderSequence := aParseTree.
	[ nodeUnderSequence parent isSequence ] whileFalse: [ nodeUnderSequence := nodeUnderSequence parent ].
	nodeUnderSequence parent
		addNodes: (statements copyFrom: 1 to: (statements size - 1 max: 0)) before: nodeUnderSequence;
		addTemporariesNamed: inlineParseTree body temporaryNames.
	aParseTree parent replaceNode: aParseTree withNode: (statements isEmpty
			 ifTrue: [ RBVariableNode selfNode ]
			 ifFalse: [ statements last ])
]

{ #category : 'adding' }
RBInlineMethodTransformation >> insertInlinedMethod [
	| node |

	node := sourceMessage.
	self moveComments.
	node parent isCascade
		ifTrue:
			[self rewriteCascadedMessage.
			node := node parent].
	node parent isReturn
		ifTrue: [node := node parent]
		ifFalse: [inlineParseTree := self removeReturnsOf: inlineParseTree].
	self replaceArguments.
	self inlineSourceReplacing: node.
	sourceParseTree removeDeadCode.
	self removeEmptyIfTrues.
	self removeImmediateBlocks
]

{ #category : 'adding' }
RBInlineMethodTransformation >> isOverridden [
	| aSelector |
	aSelector := self inlineSelector.
	class allSubclassesDo: [:each |
		(each directlyDefinesMethod: aSelector)
			ifTrue: [ ^ true ]].
	^ false
]

{ #category : 'adding' }
RBInlineMethodTransformation >> isPrimitive [
	^inlineParseTree isPrimitive
]

{ #category : 'adding' }
RBInlineMethodTransformation >> moveComments [
	inlineParseTree nodesDo:
			[:each |
			each
				comments: (each comments collect:
							[:aComment |
							| start source |
							source := sourceParseTree source.
							start := source size + 1.
							source := source
										, (inlineParseTree source copyFrom: aComment start to: aComment stop).
							sourceParseTree source: source.
							RBComment with: aComment contents at: start])]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> normalizeIfTrues [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replace: '| `@temps | ``@.s1. ``@boolean ifTrue: [| `@t1 | ``@.Stmts1. ^`@r1]. ``@.s2. ^``@r2'
			with: '| `@temps | ``@.s1. ``@boolean ifTrue: [| `@t1 | ``@.Stmts1. ^`@r1] ifFalse: [``@.s2. ^``@r2]';
		replace: '| `@temps | ``@.s1. ``@boolean ifFalse: [| `@t1 | ``@.Stmts1. ^`@r1]. ``@.s2. ^``@r2'
			with: '| `@temps | ``@.s1. ``@boolean ifTrue: [``@.s2. ^``@r2] ifFalse: [| `@t1 | ``@.Stmts1. ^`@r1]'.
	[rewriter executeTree: inlineParseTree]
		whileTrue: [inlineParseTree := rewriter tree]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> normalizeReturns [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replace: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ^``@r1] ifFalse: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ^``@r1] ifTrue: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ^``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ^``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '``@boolean ifTrue: [| `@t1 | `@.Stmts1. ^``@r1] ifFalse: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '``@boolean ifFalse: [| `@t1 | `@.Stmts1. ^``@r1] ifTrue: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]'.
	[rewriter executeTree: inlineParseTree]
		whileTrue: [inlineParseTree := rewriter tree]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> parseInlineMethod [

	self classOfTheMethodToInline
		ifNil: [ self
				refactoringError:
					( '<1p> or its superclasses don''t contain method <2s>'
						expandMacrosWith: class
						with: self inlineSelector )
			].
	inlineParseTree := self classOfTheMethodToInline parseTreeForSelector: self inlineSelector.
	inlineParseTree ifNil: [ self refactoringError: 'Could not parse sources' ].
	inlineParseTree lastIsReturn
		ifFalse: [ inlineParseTree addSelfReturn ]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> privateTransform [
	self
		renameConflictingTemporaries;
		insertInlinedMethod;
		compileMethod
]

{ #category : 'adding' }
RBInlineMethodTransformation >> removeEmptyIfTrues [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replace: '``@boolean ifTrue: [] ifFalse: [| `@temps | ``@.Stmts]'
			with: '``@boolean ifFalse: [|`@temps | ``@.Stmts]';
		replace: '``@boolean ifFalse: [] ifTrue: [| `@temps | ``@.Stmts]'
			with: '``@boolean ifTrue: [|`@temps | ``@.Stmts]';
		replace: '``@boolean ifTrue: [| `@temps | ``@.Stmts] ifFalse: []'
			with: '``@boolean ifTrue: [|`@temps | ``@.Stmts]';
		replace: '``@boolean ifFalse: [| `@temps | ``@.Stmts] ifTrue: []'
			with: '``@boolean ifFalse: [|`@temps | ``@.Stmts]'.
	(rewriter executeTree: sourceParseTree)
		ifTrue: [sourceParseTree := rewriter tree]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> removeImmediateBlocks [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replace: '[``.object] value'
		with: '``.object'
		when: [:aNode | aNode parent isCascade not].
	rewriter
		replace: '| `@temps | ``@.Stmts1. [| `@bTemps | ``@.bStmts] value. ``@.Stmts2'
		with: '| `@temps `@bTemps | ``@.Stmts1. ``@.bStmts. ``@.Stmts2'.
	(rewriter executeTree: sourceParseTree)
		ifTrue: [sourceParseTree := rewriter tree]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> removeReturnsOf: parseTree [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter replace: '^``@object' with: '``@object'.
	(rewriter executeTree: parseTree)
		ifTrue: [ ^ rewriter tree].
	^ parseTree
]

{ #category : 'adding' }
RBInlineMethodTransformation >> renameConflictingTemporaries [
	inlineParseTree allDefinedVariables
		do: [:each | self renameConflictingTemporary: each]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> renameConflictingTemporary: aName [
	| allNames newName index seqNode |
	allNames := (Set new)
				addAll: inlineParseTree allDefinedVariables;
				yourself.
	allNames remove: aName ifAbsent: [].
	seqNode := sourceMessage.
	[seqNode isSequence] whileFalse: [seqNode := seqNode parent].
	allNames addAll: seqNode allDefinedVariables.	"Add those variables defined in blocks. This might cause a few
													variables to be renamed that don't need to be, but this should be safe."
	newName := aName.
	index := 0.

	[(sourceMessage whoDefines: newName) isNotNil or:
			[(class hierarchyDefinesVariable: newName) or: [allNames includes: newName]]]
			whileTrue:
				[index := index + 1.
				newName := aName , index printString].
	newName = aName ifFalse: [self renameTemporary: aName to: newName].
	^newName
]

{ #category : 'adding' }
RBInlineMethodTransformation >> renameTemporary: oldName to: newName [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replace: oldName with: newName;
		replaceArgument: oldName with: newName.
	(rewriter executeTree: inlineParseTree)
		ifTrue: [inlineParseTree := rewriter tree]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> replaceArgument: sourceNode with: replacementNode [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter replaceTree: sourceNode withTree: replacementNode.
	(rewriter executeTree: inlineParseTree body)
		ifTrue: [inlineParseTree body: rewriter tree]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> replaceArguments [
	sourceMessage arguments reversed
		with: inlineParseTree arguments reversed
		do: [ :replacement :source |
			(replacement isImmediateNode or: [ self shouldInlineExpression: replacement newSource ])
				ifTrue: [ self replaceArgument: source with: replacement ]
				ifFalse: [ self addTemporary: source assignedTo: replacement ] ]
]

{ #category : 'adding' }
RBInlineMethodTransformation >> rewriteCascadedMessage [
	| index messages |

	messages := sourceMessage parent messages.
	index := (1 to: messages size)
				detect: [:i | sourceMessage == (messages at: i)]
				ifNone: [0].
	inlineParseTree body addNodesFirst: (messages copyFrom: 1 to: index - 1).
	inlineParseTree := self removeReturnsOf: inlineParseTree.
	inlineParseTree body
		addNodes: (messages copyFrom: index + 1 to: messages size).
	inlineParseTree addReturn
]

{ #category : 'adding' }
RBInlineMethodTransformation >> rewriteInlinedTree [
	sourceMessage parent isReturn
		ifTrue:
			[(sourceParseTree isLast: sourceMessage parent)
				ifFalse: [self addSelfReturn]]
		ifFalse:
			[self
				writeGuardClauses;
				normalizeIfTrues;
				normalizeReturns;
				addSelfReturn]
]

{ #category : 'storing' }
RBInlineMethodTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' inline: '.
	sourceInterval storeOn: aStream.
	aStream
		nextPutAll: ' inMethod: #';
		nextPutAll: sourceSelector;
		nextPutAll: ' forClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'adding' }
RBInlineMethodTransformation >> writeGuardClauses [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2. ^`@r2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1] ifFalse: [`@.s2. ^`@r2]';
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2. ^`@r2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [`@.s2. ^`@r2] ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]';
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1] ifFalse: [`@.s2. ^self]';
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [`@.s2. ^self] ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]'.
	[rewriter executeTree: self inlineParseTree]
		whileTrue: [inlineParseTree := rewriter tree]
]
