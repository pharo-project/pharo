"
I am a refactoring for replacing method calls by the method implementation.

You can select a message send in a method and refactoring this message send to inline its code.
Any temporary variable used in the original message send is added  into this method and renamed if there are already variables with this name.

My preconditions verify that the inlined method is not a primitive call, the method does not have multiple returns. I'll show a warning if the method is overriden in subclasses.


"
Class {
	#name : 'RBInlineMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'sourceInterval',
		'inlineParseTree',
		'sourceParseTree',
		'sourceSelector',
		'sourceMessage',
		'checkOverridden',
		'classOfTheMethodToInline'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBInlineMethodRefactoring class >> inline: anInterval inMethod: aSelector forClass: aClass [
	"Use this API because intervals disambiguate expression that appears twice in a method."
	
	^ self new
		inline: anInterval
		inMethod: aSelector
		forClass: aClass
]

{ #category : 'instance creation' }
RBInlineMethodRefactoring class >> model: aRBSmalltalk inline: anInterval inMethod: aSelector forClass: aClass [
	"Use this API because intervals disambiguate expression that appears twice in a method."
	
	^ self new
		model: aRBSmalltalk;
		inline: anInterval
			inMethod: aSelector
			forClass: aClass;
		yourself
]

{ #category : 'testing - api' }
RBInlineMethodRefactoring class >> model: aRBSmalltalk inlineSource: aSourceCode fromMethod: aSelector inClass: aClassName [
	"This API is for testing purposes only. If you use it and there are two similar expressions
	(selection) in the method, the first one will be extracted every time."
	
	^ self new
		model: aRBSmalltalk;
		inlineSource: aSourceCode
		fromMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> addSelfReturn [
	inlineParseTree addSelfReturn
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> addTemporary: sourceNode assignedTo: replacementNode [
	| newName |
	newName := self renameConflictingTemporary: sourceNode name.
	(inlineParseTree body)
		addTemporaryNamed: newName;
		addNodeFirst: (OCAssignmentNode variable: (OCVariableNode named: newName)
					value: replacementNode)
]

{ #category : 'preconditions' }
RBInlineMethodRefactoring >> applicabilityPreconditions [
	"Even though it looks complex it only does preconditions and set up. `transform` does actual refactoring"

	^ {
		  (RBCondition definesSelector: sourceSelector in: class).
		  (RBCondition withBlock: [
			   self checkSelectedMessage.
			   self parseInlineMethod.
			   inlineParseTree isPrimitive ifTrue: [
				   self refactoringError: 'Cannot inline primitives' ].
			   self checkSuperMessages.
			   self rewriteInlinedTree.
			   (sourceMessage parent isReturn or: [
				    self hasMultipleReturns not ]) ifFalse: [
				   self refactoringError:
					   'Cannot inline method since it contains multiple returns that cannot be rewritten' ].
			   true ]) }
]

{ #category : 'preconditions' }
RBInlineMethodRefactoring >> breakingChangePreconditions [

	^ { (RBCondition withBlock: [
		   self checkOverridden ifTrue: [
			   self isOverridden ifTrue: [
				   self refactoringWarning:
					   ('<1p>>><2s> is overriden. Do you want to inline it anyway?'
					    , String cr , 'You can break you hooks with this inline.'
						    expandMacrosWith: self classOfTheMethodToInline
						    with: self inlineSelector) ] ].
		   true ]) }
]

{ #category : 'accessing' }
RBInlineMethodRefactoring >> checkOverridden [
	^ checkOverridden ifNil: [ checkOverridden := true ]
]

{ #category : 'accessing' }
RBInlineMethodRefactoring >> checkOverridden: aBoolean [
	checkOverridden := aBoolean
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> checkSelectedMessage [

	sourceParseTree := class parseTreeForSelector: sourceSelector.
	sourceParseTree ifNil: [
		self refactoringError: 'Could not parse sources' ].
	sourceMessage := sourceParseTree whichNodeIsContainedBy:
		                 sourceInterval.
	sourceMessage ifNil: [
		self refactoringError:
			'The selection doesn''t appear to be a message send' ].
	sourceMessage isCascade ifTrue: [
		sourceMessage := sourceMessage messages last ].
	sourceMessage isMessage ifFalse: [
		self refactoringError:
			'The selection doesn''t appear to be a message send' ].
	(sourceMessage receiver isSelfVariable or: [
		 sourceMessage receiver isSuperVariable ]) ifFalse: [
		self refactoringError: 'Cannot inline non-self messages' ]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> checkSuperMessages [

	self classOfTheMethodToInline = class ifTrue: [ ^ self ].
	self classOfTheMethodToInline superclass ifNil: [ ^ self ].
	inlineParseTree superMessages do: [ :each |
		(self classOfTheMethodToInline superclass
			 whichClassIncludesSelector: each)
		= (class superclass whichClassIncludesSelector: each) ifFalse: [
			self refactoringError:
				('Cannot inline method since it sends a super message <1s> that is overriden'
					 expandMacrosWith: each) ] ]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> classOfTheMethodToInline [

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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> compileMethod [

	class compileTree: sourceParseTree
]

{ #category : 'only for testing' }
RBInlineMethodRefactoring >> findFirstOccurrenceOf: searchedString in: textToSearchIn [
	"Return the first index of aString in textToSearchIn "
	| firstIndex |
	firstIndex := textToSearchIn findString: searchedString startingAt: 1.
	[ (firstIndex > 1) and: [ (textToSearchIn at: (firstIndex - 1)) isAlphaNumeric ] ]
		whileTrue: [
			firstIndex := textToSearchIn findString: searchedString startingAt: firstIndex +1 ].
		
	^ firstIndex
]

{ #category : 'testing' }
RBInlineMethodRefactoring >> hasMultipleReturns [
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

{ #category : 'initialization' }
RBInlineMethodRefactoring >> inline: anInterval inMethod: aSelector forClass: aClass [
	sourceSelector := aSelector.
	class := self classObjectFor: aClass.
	sourceInterval := anInterval
]

{ #category : 'accessing' }
RBInlineMethodRefactoring >> inlineParseTree [
	^ inlineParseTree
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> inlineSelector [

	^ sourceMessage selector
]

{ #category : 'only for testing' }
RBInlineMethodRefactoring >> inlineSource: messageSendSource fromMethod: aSelector inClass: aClassName [
	
	sourceSelector := aSelector.
	class := self model classNamed: aClassName.
	sourceInterval := self sourceIntervalForMessageSend: messageSendSource
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> inlineSourceReplacing: aParseTree [
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
			 ifTrue: [ OCVariableNode selfNode ]
			 ifFalse: [ statements last ])
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> insertInlinedMethod [
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

{ #category : 'testing' }
RBInlineMethodRefactoring >> isOverridden [

	^ class subclassRedefines: self inlineSelector
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> moveComments [
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
							OCCommentNode with: aComment contents at: start])]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> normalizeIfTrues [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> normalizeReturns [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> parseInlineMethod [

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

{ #category : 'preconditions' }
RBInlineMethodRefactoring >> preconditions [ 

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> privateTransform [
	self
		renameConflictingTemporaries;
		insertInlinedMethod;
		compileMethod
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> removeEmptyIfTrues [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> removeImmediateBlocks [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> renameConflictingTemporaries [
	inlineParseTree allDefinedVariables
		do: [:each | self renameConflictingTemporary: each]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> renameConflictingTemporary: aName [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> renameTemporary: oldName to: newName [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter
		replace: oldName with: newName;
		replaceArgument: oldName with: newName.
	(rewriter executeTree: inlineParseTree)
		ifTrue: [inlineParseTree := rewriter tree]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> replaceArgument: sourceNode with: replacementNode [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter replaceTree: sourceNode withTree: replacementNode.
	(rewriter executeTree: inlineParseTree body)
		ifTrue: [inlineParseTree body: rewriter tree]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> replaceArguments [
	sourceMessage arguments reversed
		with: inlineParseTree arguments reversed
		do: [ :replacement :source |
			self flag: #todo. "shouldNotCreateExtraBindings can produce bogus transformations.
			Mainly by evaluating multiple time expressions. We can fix this by ensuring that
			binding creation is only done when the argument expression (or self) is used multiple times.
			This will avoid that ProtectField refactoring uses shouldNotCreateExtraBindings."
			(replacement isImmediateNode or: [ self shouldNotCreateExtraBindings: replacement newSource ])
				ifTrue: [ self replaceArgument: source with: replacement ]
				ifFalse: [ self addTemporary: source assignedTo: replacement ] ]
]

{ #category : 'transforming' }
RBInlineMethodRefactoring >> rewriteCascadedMessage [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> rewriteInlinedTree [
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

{ #category : 'only for testing' }
RBInlineMethodRefactoring >> sourceIntervalForMessageSend: messageSendSource [ 

	| rbMethod start |
	rbMethod := class methodFor: sourceSelector.
	start := self findFirstOccurrenceOf: messageSendSource in: rbMethod ast sourceCode.
	^ start to: (start + messageSendSource size)
]

{ #category : 'accessing' }
RBInlineMethodRefactoring >> sourceSelector [
	^ sourceSelector
]

{ #category : 'storing' }
RBInlineMethodRefactoring >> storeOn: aStream [
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

{ #category : 'transforming' }
RBInlineMethodRefactoring >> writeGuardClauses [
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
