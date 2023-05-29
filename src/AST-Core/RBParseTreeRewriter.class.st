"
ParseTreeRewriter walks over and transforms its RBProgramNode (tree). If the tree is modified, then the answer is set to true, and the modified tree can be retrieved by the #tree method.


Here is a little script to rewrite a self halt into self dormantHalt. 

	| rewriter node |
	rewriter := RBParseTreeRewriter new.
	rewriter replace: 'self halt' with: 'self dormatHalt'.
	node := (ProtoObjectTest>>#testIfNil) parseTree.
	rewriter executeTree: node.
	^ node formattedCode

Note how do we get the transformed code. You can access the rewritten tree as follows:
rewriter tree.

Now here is a full script showing how to compile the method back. 

   | rewriter ok method |
	rewriter := RBParseTreeRewriter new.
	rewriter replaceMethod: self searchPattern with: self targetPattern.
	method := (BIArrayExpressionTest>>#testNoExtraSpaceAroundPeriod).
	ok := rewriter executeTree: method parseTree.
	ok ifFalse: [ ^ 'did not work' ].
	Author 
		useAuthor: 'Refactoring'
		during: [  
			method origin 
				compile: rewriter tree formattedCode 
				classified: method protocol ]


Have a look at the users of deprecated:

		deprecated: 'Please use #isPinnedInMemory instead'
		transformWith: '`@receiver isPinned' -> '`@receiver isPinnedInMemory'.

You can also have a look at the ParseTreeRewriterTest class.


Instance Variables:
	tree	<RBProgramNode>	the parse tree we're transforming
			
	
"
Class {
	#name : #RBParseTreeRewriter,
	#superclass : #RBParseTreeSearcher,
	#instVars : [
		'tree'
	],
	#category : #'AST-Core-Matching'
}

{ #category : #'instance creation' }
RBParseTreeRewriter class >> removeTemporaryNamed: aName [
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule replace: '| `@temps1 ' , aName , ' `@temps2 | ``@.Statements'
		with: '| `@temps1  `@temps2 | ``@.Statements'.
	^rewriteRule
]

{ #category : #'instance creation' }
RBParseTreeRewriter class >> rename: varName to: newVarName [
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: varName with: newVarName;
		replaceArgument: varName with: newVarName.
	^rewriteRule
]

{ #category : #'instance creation' }
RBParseTreeRewriter class >> rename: varName to: newVarName handler: aBlock [
	"Rename varName to newVarName, evaluating aBlock if there is a
	temporary variable with the same name as newVarName. This
	does not change temporary variables with varName."

	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: varName with: newVarName;
		replaceArgument: newVarName
			withValueFrom:
				[:aNode |
				aBlock value.
				aNode].
	^rewriteRule
]

{ #category : #accessing }
RBParseTreeRewriter class >> replace: code with: newCode in: aParseTree [
	^(self
		replace: code
		with: newCode
		method: false)
		executeTree: aParseTree;
		tree
]

{ #category : #accessing }
RBParseTreeRewriter class >> replace: code with: newCode in: aParseTree onInterval: anInterval [
	| rewriteRule |
	rewriteRule := self new.
	^rewriteRule
		replace: code
			with: newCode
			when: [:aNode | aNode intersectsInterval: anInterval];
		executeTree: aParseTree;
		tree
]

{ #category : #'instance creation' }
RBParseTreeRewriter class >> replace: code with: newCode method: aBoolean [
	| rewriteRule |
	rewriteRule := self new.
	aBoolean
		ifTrue: [rewriteRule replaceMethod: code with: newCode]
		ifFalse: [rewriteRule replace: code with: newCode].
	^rewriteRule
]

{ #category : #'instance creation' }
RBParseTreeRewriter class >> replaceLiteral: literal with: newLiteral [
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule replaceTree: (RBLiteralNode value: literal)
		withTree: (RBLiteralNode value: newLiteral).
	^rewriteRule
]

{ #category : #accessing }
RBParseTreeRewriter class >> replaceStatements: code with: newCode in: aParseTree onInterval: anInterval [
	| tree replaceStmt |
	tree := self buildTree: code method: false.
	tree isSequence ifFalse: [ tree := RBSequenceNode statements: (Array with: tree) ].
	tree temporaries: (Array with: (RBPatternVariableNode named: '`@temps')).
	tree addNodeFirst: (RBPatternVariableNode named: '`@.S1').
	replaceStmt := tree lastIsReturn
		ifTrue: [ '| `@temps | `@.S1. ^' , newCode ]
		ifFalse: [ tree addNode: (RBPatternVariableNode named: '`@.S2').
			'| `@temps | `@.S1. ' , newCode , '. `@.S2' ].
	^ self
		replace: tree formattedCode
		with: replaceStmt
		in: aParseTree
		onInterval: anInterval
]

{ #category : #'instance creation' }
RBParseTreeRewriter class >> variable: aVarName getter: getMethod setter: setMethod [
	^self
		variable: aVarName
		getter: getMethod
		setter: setMethod
		receiver: 'self'
]

{ #category : #'instance creation' }
RBParseTreeRewriter class >> variable: aVarName getter: getMethod setter: setMethod receiver: aString [
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: aVarName , ' := ``@object'
			with: aString , ' ' , setMethod , ' ``@object';
		replace: aVarName with: aString , ' ' , getMethod.
	^rewriteRule
]

{ #category : #accessing }
RBParseTreeRewriter >> executeTree: aParseTree [
	"Replace the argument node based on the replace rules. Return false when no transformation has been applied, and true when a transformation occured.

	Pay attention the method is not recompiled. Just the tree is modified. Look at class comment to see how the method can be compiled"

	"here is a little script showing a possible way to use executeTree:
	| rewriter node |
	rewriter := RBParseTreeRewriter new.
	rewriter replace: 'self halt' with: 'self dormatHalt'.
	node := (ProtoObjectTest>>#testIfNil) parseTree.
	rewriter executeTree: node.
	^ node formattedCode
"


	| oldContext |
	oldContext := context.
	context := SmallDictionary new.
	answer := false.
	tree := self visitNode: aParseTree.
	context := oldContext.
	^answer
]

{ #category : #private }
RBParseTreeRewriter >> foundMatch [
	"Set that the pattern matched is a successful"

	answer := true
]

{ #category : #private }
RBParseTreeRewriter >> lookForMoreMatchesInContext: oldContext [
	oldContext keysAndValuesDo: [:key :value |
		(key isString not and: [key recurseInto])
			ifTrue:
				[oldContext at: key put: (value collect: [:each | self visitNode: each])]]
]

{ #category : #replacing }
RBParseTreeRewriter >> replace: searchString with: replaceString [
   "Add a new replace pattern. To get the replacement executed invoke executeTree: method."

	self addRule: (RBStringReplaceRule searchFor: searchString
				replaceWith: replaceString)
]

{ #category : #replacing }
RBParseTreeRewriter >> replace: searchString with: replaceString when: aBlock [
	"Add a new replace pattern when condition is true. To get the replacement executed invoke executeTree: method."

	self addRule: (RBStringReplaceRule
				searchFor: searchString
				replaceWith: replaceString
				when: aBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replace: searchString withValueFrom: replaceBlock [
	self addRule: (RBBlockReplaceRule searchFor: searchString
				replaceWith: replaceBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replace: searchString withValueFrom: replaceBlock when: conditionBlock [
	self addRule: (RBBlockReplaceRule
				searchFor: searchString
				replaceWith: replaceBlock
				when: conditionBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceArgument: searchString with: replaceString [
	self addArgumentRule: (RBStringReplaceRule searchFor: searchString
				replaceWith: replaceString)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceArgument: searchString with: replaceString when: aBlock [
	self addArgumentRule: (RBStringReplaceRule
				searchFor: searchString
				replaceWith: replaceString
				when: aBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceArgument: searchString withValueFrom: replaceBlock [
	self addArgumentRule: (RBBlockReplaceRule searchFor: searchString
				replaceWith: replaceBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceArgument: searchString withValueFrom: replaceBlock when: conditionBlock [
	self addArgumentRule: (RBBlockReplaceRule
				searchFor: searchString
				replaceWith: replaceBlock
				when: conditionBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceMethod: searchString with: replaceString [
	self addRule: (RBStringReplaceRule searchForMethod: searchString
				replaceWith: replaceString)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceMethod: searchString with: replaceString when: aBlock [
	self addRule: (RBStringReplaceRule
				searchForMethod: searchString
				replaceWith: replaceString
				when: aBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceMethod: searchString withValueFrom: replaceBlock [
	self addRule: (RBBlockReplaceRule searchForMethod: searchString
				replaceWith: replaceBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceMethod: searchString withValueFrom: replaceBlock when: conditionBlock [
	self addRule: (RBBlockReplaceRule
				searchForMethod: searchString
				replaceWith: replaceBlock
				when: conditionBlock)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceTree: searchTree withTree: replaceTree [
	"Add a replacement between a given tree and a replacement tree. To get the replacement executed invoke executeTree: method."

	self addRule: (RBStringReplaceRule searchForTree: searchTree
				replaceWith: replaceTree)
]

{ #category : #replacing }
RBParseTreeRewriter >> replaceTree: searchTree withTree: replaceTree when: aBlock [
	"Add a replacement between a given tree and a replacement tree when a condition. To get the replacement executed invoke executeTree: method."

	self addRule: (RBStringReplaceRule
				searchForTree: searchTree
				replaceWith: replaceTree
				when: aBlock)
]

{ #category : #accessing }
RBParseTreeRewriter >> tree [
	"Return the rewritten tree"

	^tree
]

{ #category : #visiting }
RBParseTreeRewriter >> visitArgumentNodes: aNodeCollection [
	^aNodeCollection collect: [:each | self visitArgumentNode: each]
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitArrayNode: anArrayNode [
	anArrayNode statements: (anArrayNode statements collect: [:each | self visitNode: each])
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitAssignmentNode: anAssignmentNode [
	anAssignmentNode variable: (self visitNode: anAssignmentNode variable).
	anAssignmentNode value: (self visitNode: anAssignmentNode value)
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitBlockNode: aBlockNode [
	aBlockNode arguments: (self visitArgumentNodes: aBlockNode arguments).
	aBlockNode body: (self visitNode: aBlockNode body)
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitCascadeNode: aCascadeNode [

	| newMessages notFound |

	newMessages := OrderedCollection new: aCascadeNode messages size.
	notFound := OrderedCollection new: aCascadeNode messages size.
	aCascadeNode messages
		do: [ :each |
			| newNode |

			newNode := self performSearches: searches on: each.
			newNode
				ifNil: [ newNode := each.
					notFound add: newNode
					].
			newNode isMessage
				ifTrue: [ newMessages add: newNode ]
				ifFalse: [ newNode isCascade
						ifTrue: [ newMessages addAll: newNode messages ]
						ifFalse: [ "Cannot replace message node inside of cascaded node with non-message node."
							self flag: #pharoFixMe.	"Maybe we should raise a warning here and catch it in the UI layer."
							newMessages add: each
							]
					]
			].
	notFound size = aCascadeNode messages size
		ifTrue: [ | receiver |

			receiver := self visitNode: aCascadeNode messages first receiver.
			newMessages do: [ :each | each receiver: receiver ]
			].
	notFound do: [ :each | each arguments: ( each arguments collect: [ :arg | self visitNode: arg ] ) ].
	aCascadeNode messages: newMessages
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitLiteralArrayNode: aRBArrayLiteralNode [
	aRBArrayLiteralNode contents: (aRBArrayLiteralNode contents collect: [ :each | self visitNode: each ])
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitMessageNode: aMessageNode [
	aMessageNode receiver: (self visitNode: aMessageNode receiver).
	aMessageNode arguments: (aMessageNode arguments collect: [ :each | self visitNode: each ])
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitMethodNode: aMethodNode [
	aMethodNode arguments: (self visitArgumentNodes: aMethodNode arguments).
	aMethodNode pragmas: (aMethodNode pragmas collect: [ :each | self visitNode: each ]).
	aMethodNode body: (self visitNode: aMethodNode body)
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments: (aPragmaNode arguments collect: [ :each | self visitNode: each ])
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitReturnNode: aReturnNode [
	aReturnNode value: (self visitNode: aReturnNode value)
]

{ #category : #'visitor - double dispatching' }
RBParseTreeRewriter >> visitSequenceNode: aSequenceNode [
	aSequenceNode temporaries: (self visitTemporaryNodes: aSequenceNode temporaries).
	aSequenceNode statements: (aSequenceNode statements collect: [ :each | self visitNode: each ])
]

{ #category : #visiting }
RBParseTreeRewriter >> visitTemporaryNodes: aNodeCollection [
	^aNodeCollection collect: [:each | self visitTemporaryDeclarationNode: each]
]
