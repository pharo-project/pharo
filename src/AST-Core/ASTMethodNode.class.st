"
ASTMethodNode is the node that represents AST of a Smalltalk method.

Some properties aren't known to the parser creating this Object. For example, the scope value isn't known by parsing the code but only after doing a
semantic analysis. Likewise, the compilation context isn't needed until we try to do the semantic analysis. 

Instance Variables:
	arguments	<SequenceableCollection of: ASTVariableNode>	the arguments to the method
	body	<BRSequenceNode>	the body/statements of the method
	nodeReplacements	<Dictionary>	a dictionary of oldNode -> newNode replacements
	replacements	<Collection of: ASTStringReplacement>	the collection of string replacements for each node replacement in the parse tree
	selector	<Symbol>	the method name
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords
	source	<String>	the source we compiled
	scope	<OCMethodScope | nil> the scope associated with this code of this method
	pragmas	< SequenceableCollection of: ASTPragmaNodes > Nodes representing the pragma statements in this method
	compilationContext	<CCompilationContext | CompilationContext>

"
Class {
	#name : 'ASTMethodNode',
	#superclass : 'ASTProgramNode',
	#instVars : [
		'scope',
		'selector',
		'keywordsPositions',
		'body',
		'source',
		'arguments',
		'pragmas',
		'replacements',
		'nodeReplacements',
		'compilationContext',
		'bcToASTCache'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
ASTMethodNode class >> selector: aSymbol [
	^self selector: aSymbol arguments: #()
]

{ #category : 'instance creation' }
ASTMethodNode class >> selector: aSymbol arguments: valueNodes [
	^(self new)
		selector: aSymbol;
		arguments: valueNodes;
		yourself
]

{ #category : 'instance creation' }
ASTMethodNode class >> selector: aSymbol arguments: variableNodes body: aSequenceNode [
	^ self new 
		arguments: variableNodes;
		selector: aSymbol;
		body: aSequenceNode;
		yourself
]

{ #category : 'instance creation' }
ASTMethodNode class >> selector: aSymbol body: aSequenceNode [
	^self
		selector: aSymbol
		arguments: #()
		body: aSequenceNode
]

{ #category : 'instance creation' }
ASTMethodNode class >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	^(self new)
		selector: aSymbol
			keywordsPositions: positionList
			arguments: valueNodes;
		yourself
]

{ #category : 'comparing' }
ASTMethodNode >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	(self selector = anObject selector
		and: [ self pragmas size = anObject pragmas size
		and: [ self body = anObject body ] ])
			ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		first = second
			ifFalse: [ ^ false ] ].
	self pragmas with: anObject pragmas do: [ :first :second |
		first = second
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'visiting' }
ASTMethodNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitMethodNode: self
]

{ #category : 'adding-removing' }
ASTMethodNode >> addNode: aNode [
	^ body addNode: aNode
]

{ #category : 'adding-removing' }
ASTMethodNode >> addPragma: aPragmaNode [

	pragmas := pragmas copyWith: aPragmaNode.
	aPragmaNode parent: self
]

{ #category : 'replacing' }
ASTMethodNode >> addReplacement: aStringReplacement [
	replacements ifNil: [^self].
	replacements add: aStringReplacement
]

{ #category : 'adding-removing' }
ASTMethodNode >> addReturn [
	^ body addReturn
]

{ #category : 'adding-removing' }
ASTMethodNode >> addSelfReturn [
	^ body addSelfReturn
]

{ #category : 'accessing' }
ASTMethodNode >> allArgumentVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allArgumentVariables;
		yourself
]

{ #category : 'accessing' }
ASTMethodNode >> allDefinedVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : 'iterating' }
ASTMethodNode >> allSequenceNodes [

	^ self allChildren select: #isSequence
]

{ #category : 'accessing' }
ASTMethodNode >> allStatements [
	"return the statements including variable definition."

	^ (OrderedCollection withAll: self temporaries)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
ASTMethodNode >> argumentNames [
	^ self arguments collect: [:each | each name] as: Array
]

{ #category : 'accessing' }
ASTMethodNode >> arguments [
	^arguments
]

{ #category : 'accessing' }
ASTMethodNode >> arguments: variableNodes [
	arguments := variableNodes.
	arguments do: [:each | each parent: self]
]

{ #category : 'converting' }
ASTMethodNode >> asSequenceNode [
	^body
]

{ #category : 'accessing' }
ASTMethodNode >> body [
	^body
]

{ #category : 'accessing' }
ASTMethodNode >> body: stmtsNode [
	body := stmtsNode.
	body parent: self
]

{ #category : 'private - replacing' }
ASTMethodNode >> changeSourceSelector: oldSelector keywordsIntervals: oldIntervals arguments: oldArguments [
	"If this is the same number of arguments, we try a one to one replacement of selector parts and arguments. If this is not the c
ase try to rewrite the signature as a whole, what unfortunately drops the comments within the signature."

	[ (oldIntervals size = self keywordsPositions size and: [ oldArguments size = arguments size ])
		ifTrue: [
			self selectorParts keysAndValuesDo: [ :index :part |
				self addReplacement: (ASTStringReplacement
					replaceFrom: (oldIntervals at: index) first
					to: (oldIntervals at: index) last
					with: part) ].
			oldArguments with: arguments do: [ :old :new |
				self addReplacement: (ASTStringReplacement
					replaceFrom: old start
					to: old stop
					with: new name) ] ]
		ifFalse: [
			self addReplacement: (ASTStringReplacement
				replaceFrom: oldIntervals first first
				to: (oldArguments notEmpty
					ifTrue: [ oldArguments last stop ]
					ifFalse: [ oldIntervals last last ])
				with: (String streamContents: [ :stream |
					self selectorParts keysAndValuesDo: [ :index :part |
						index = 1 ifFalse: [ stream space ].
						stream nextPutAll: part.
						index <= arguments size
							ifTrue: [ stream space; nextPutAll: (arguments at: index) name ] ] ])) ] ]
		on: Error
		do: [ :ex | ex return ]
]

{ #category : 'accessing' }
ASTMethodNode >> children [
	^ OrderedCollection new
		addAll: arguments;
		addAll: pragmas;
		add: body;
		yourself
]

{ #category : 'replacing' }
ASTMethodNode >> clearReplacements [
	replacements := nil
]

{ #category : 'accessing - compiled method' }
ASTMethodNode >> compilationContext [
	^ compilationContext
]

{ #category : 'accessing - compiled method' }
ASTMethodNode >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : 'accessing - conceptual' }
ASTMethodNode >> conceptualArgumentSize [
	"Return the cumulted length of the parameters (yes parameters are called arguments in Pharo - not good!). It does not count spaces and the selectors.
	Note that this length is based on the tree structure and not the source code. It does not reflect the selection interval but the space the selector would take if displayed on a single line without any space."

	^ self arguments
		inject: 0
		into: [ :sum :each | sum + each name size ]
]

{ #category : 'accessing - conceptual' }
ASTMethodNode >> conceptualSelectorSize [
	"Return the length of the selector. It does not take into length of parameters nor the spaces.
	Note that this length is based on the tree structure and not the source code. It does not reflect the selection interval but the space the selector would take if displayed on a single line without any space."

	^ selector size
]

{ #category : 'accessing - conceptual' }
ASTMethodNode >> conceptualSignatureSize [
	"Return the full length of the method signature (called with a strange name in Pharo: the patterned selector. Note that this length is based on the tree structure and not the source code. It does not reflect the selection interval but the space the signature would take if displayed on a single line."

	"When we count foo: a b: b, we add three extras spaces so args size * 2 - 1"

	^ self conceptualSelectorSize + self conceptualArgumentSize
		+ (self arguments
		ifNotEmpty: [ self arguments size * 2 - 1 ]
		ifEmpty: [ 0 ])
]

{ #category : 'matching' }
ASTMethodNode >> copyInContext: aDictionary [
	^ self class new
		selector: self selector;
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ]);
		pragmas: (self pragmas isEmpty
			ifTrue: [ aDictionary at: '-pragmas-' ifAbsent: [ #() ] ]
			ifFalse: [ self copyList: self pragmas inContext: aDictionary ]);
		body: (self body copyInContext: aDictionary);
		source: (aDictionary at: '-source-' ifAbsentPut: [ self source ]);
		yourself
]

{ #category : 'testing' }
ASTMethodNode >> defines: aName [
	^ (arguments anySatisfy: [ :each | each name = aName ])
		or: [ self pragmas anySatisfy: [ :pragma | pragma defines: aName ] ]
]

{ #category : 'comparing' }
ASTMethodNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [ ^ false ].
	(self selector = anObject selector
		and: [ self pragmas size = anObject pragmas size
		and: [ self body equalTo: anObject body withMapping: aDictionary ] ])
			ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second	withMapping: aDictionary)
			ifFalse: [ ^ false ].
		aDictionary removeKey: first name ].
	self pragmas with: anObject pragmas do: [ :first :second |
		(first equalTo: second	withMapping: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'tree accessing' }
ASTMethodNode >> extractSubtreeWith: aSourceCode [
	| extractedTree |
	extractedTree := self parserClass
		parseExpression: aSourceCode
		onError: [ :string :pos | ^ nil ].
	extractedTree ifNil: [ ^ nil ].
	(extractedTree isSequence and: [ extractedTree statements isEmpty ])
		ifTrue: [ ^ nil ].
	extractedTree := extractedTree isSequence
		ifTrue:
			[ ASTParseTreeSearcher treeMatchingStatements: aSourceCode in: self ]
		ifFalse:
			[ ASTParseTreeSearcher treeMatching: aSourceCode in: self ].	"or: [aTree isReturn]"
	^ extractedTree
		"ifNotNil: [ extractedTree parent: (self parentOfSubtree: extractedTree) ]"
]

{ #category : 'accessing' }
ASTMethodNode >> firstPrecodeComment [
	self comments ifEmpty: [ ^nil ].
	^ self comments first contents
]

{ #category : 'testing' }
ASTMethodNode >> hasArgumentNamed: aString [
	^ self arguments anySatisfy: [ :argument| argument name = aString ]
]

{ #category : 'testing' }
ASTMethodNode >> hasArguments [

	^ arguments isNotEmpty
]

{ #category : 'testing' }
ASTMethodNode >> hasBlock [

	^ body hasBlock 
]

{ #category : 'testing' }
ASTMethodNode >> hasPragmaNamed: aSymbol [
	self pragmaNamed: aSymbol ifAbsent: [ ^ false ].
	^ true
]

{ #category : 'testing' }
ASTMethodNode >> hasTemporaries [
	^ self temporaries isNotEmpty
]

{ #category : 'testing' }
ASTMethodNode >> hasTemporaryNamed: aString [
	^ self temporaries anySatisfy: [ :temp| temp name = aString ]
]

{ #category : 'comparing' }
ASTMethodNode >> hash [
	^ ((self selector hash bitXor: (self hashForCollection: self arguments)) bitXor: (self hashForCollection: self pragmas)) bitXor: self body hash
]

{ #category : 'initialization' }
ASTMethodNode >> initialize [
	super initialize.
	arguments := #().
	pragmas := #().
	replacements := SortedCollection sortBlock:
					[:a :b |
					a startPosition < b startPosition
						or: [a startPosition = b startPosition and: [a stopPosition < b stopPosition]]].
	nodeReplacements := IdentityDictionary new
]

{ #category : 'testing' }
ASTMethodNode >> isDoIt [
	^false
]

{ #category : 'errors' }
ASTMethodNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	(self arguments anySatisfy: [:each | each isFaulty]) ifTrue:[ ^true].
	(self pragmas anySatisfy: [:each | each isFaulty]) ifTrue:[ ^true].
	^self body isFaulty
]

{ #category : 'testing' }
ASTMethodNode >> isLast: aNode [
	^body isLast: aNode
]

{ #category : 'testing' }
ASTMethodNode >> isMethod [
	^true
]

{ #category : 'testing' }
ASTMethodNode >> isPrimitive [
	^ self pragmas anySatisfy: [ :each | each isPrimitive ]
]

{ #category : 'testing' }
ASTMethodNode >> isUsingAsReturnValue: aNode [
	^body == aNode and: [aNode lastIsReturn]
]

{ #category : 'accessing' }
ASTMethodNode >> keywords [
	^ selector keywords
]

{ #category : 'accessing' }
ASTMethodNode >> keywordsIntervals [
	^selector keywords
		with: self keywordsPositions
		collect: [:keyword :start| start to: (start = 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ]
]

{ #category : 'accessing' }
ASTMethodNode >> keywordsPositions [
	^keywordsPositions ifNil: [ (selector keywords collect: [:char| 0 ]) asIntegerArray ]
]

{ #category : 'accessing' }
ASTMethodNode >> keywordsPositions: aPositionsList [
	keywordsPositions := aPositionsList ifNotNil: [:list| list asIntegerArray ]
]

{ #category : 'testing' }
ASTMethodNode >> lastIsReturn [
	^body lastIsReturn
]

{ #category : 'replacing' }
ASTMethodNode >> map: oldNode to: newNode [
	nodeReplacements at: oldNode put: newNode
]

{ #category : 'replacing' }
ASTMethodNode >> mappingFor: oldNode [
	^nodeReplacements at: oldNode ifAbsent: [oldNode]
]

{ #category : 'matching' }
ASTMethodNode >> match: aNode inContext: aDictionary [
	self class = aNode class ifFalse: [ ^ false ].
	aDictionary at: '-source-' put: aNode source.
	self selector = aNode selector ifFalse: [ ^ false ].
	^ (self matchList: arguments against: aNode arguments inContext: aDictionary)
		and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
		and: [ body match: aNode body inContext: aDictionary ] ]
]

{ #category : 'matching' }
ASTMethodNode >> matchPragmas: matchNodes against: pragmaNodes inContext: aDictionary [
	matchNodes isEmpty ifTrue: [
		aDictionary at: '-pragmas-' put: pragmaNodes.
		^ true ].
	^ matchNodes allSatisfy: [ :matchNode |
		pragmaNodes anySatisfy: [ :pragmaNode |
			matchNode match: pragmaNode inContext: aDictionary ] ]
]

{ #category : 'accessing' }
ASTMethodNode >> methodNode [
	^self
]

{ #category : 'accessing' }
ASTMethodNode >> methodOrBlockNode [
	^ self
]

{ #category : 'accessing' }
ASTMethodNode >> newSource [
	replacements ifNil: [^self formattedCode].
	^[self reformatSource] on: Error do: [:ex | ex return: self formattedCode]
]

{ #category : 'accessing' }
ASTMethodNode >> numArgs [
	^self selector numArgs
]

{ #category : 'testing' }
ASTMethodNode >> offsetIsComment: anOffset [
	"check is the offset in the source part of a comment"

	^ self allComments
		anySatisfy: [ :comment | anOffset between: comment start and: comment stop ]
]

{ #category : 'copying' }
ASTMethodNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self pragmas: (self pragmas collect: [ :each | each copy ]).
	self body: self body copy
]

{ #category : 'testing' }
ASTMethodNode >> pragmaNamed: aSymbol [
	^ self pragmaNamed: aSymbol ifAbsent: [ KeyNotFound signalFor: aSymbol  ]
]

{ #category : 'testing' }
ASTMethodNode >> pragmaNamed: aSymbol ifAbsent: absentBlock [
	^ self pragmas
		detect: [ :pragma| pragma selector = aSymbol ]
		ifNone: absentBlock
]

{ #category : 'testing' }
ASTMethodNode >> pragmaNamed: aSymbol ifPresent: foundBlock [
	^ self pragmas
		detect: [ :pragma| pragma selector = aSymbol ]
		ifFound: foundBlock
]

{ #category : 'testing' }
ASTMethodNode >> pragmaNamed: aSymbol ifPresent: presentBlock ifAbsent: absentBlock [
	^ self pragmas
		detect: [ :pragma| pragma selector = aSymbol ]
		ifFound: presentBlock
		ifNone: absentBlock
]

{ #category : 'accessing' }
ASTMethodNode >> pragmas [
	^ pragmas
]

{ #category : 'accessing' }
ASTMethodNode >> pragmas: aCollection [
	pragmas := aCollection.
	pragmas do: [ :each | each parent: self ]
]

{ #category : 'printing' }
ASTMethodNode >> printOn: aStream [
	aStream nextPutAll: self formattedCode
]

{ #category : 'testing' }
ASTMethodNode >> references: aVariableName [
	^body references: aVariableName
]

{ #category : 'private' }
ASTMethodNode >> reformatSource [
	| stream newSource newTree |
	stream := (String new: source size + 100) writeStream.
	stream
		nextPutAll:
			(source
				copyFrom:
					(replacements
						inject: 1
						into: [ :sum :each |
							stream
								nextPutAll: (source copyFrom: sum to: each startPosition - 1);
								nextPutAll: each string.
							each stopPosition + 1 ])
				to: source size).
	newSource := stream contents.
	newTree := self parserClass parseFaultyMethod: newSource.
	self = newTree
		ifFalse: [ ^ self formattedCode ].
	^ newSource
]

{ #category : 'adding-removing' }
ASTMethodNode >> removePragma: aPragmaNode [

	pragmas := pragmas copyWithout: aPragmaNode
]

{ #category : 'adding-removing' }
ASTMethodNode >> removePragmaNamed: aPragmaName [

	self pragmaNamed: aPragmaName ifPresent: [ :pragma | self removePragma: pragma ]
]

{ #category : 'adding-removing' }
ASTMethodNode >> removeSubtree: aTree [

	^ aTree isReturn
		ifTrue: [ ASTParseTreeRewriter
					 replace: aTree formattedCode
					 with: '' in: self
					 onInterval: aTree sourceInterval ]
		ifFalse: [ ASTParseTreeRewriter
					  replaceStatements: aTree formattedCode
		 			  with: '' in: self
					  onInterval: aTree sourceInterval ]
]

{ #category : 'accessing' }
ASTMethodNode >> renameSelector: newSelector andArguments: varNodeCollection [
	| oldIntervals oldArguments oldSelector |
	oldSelector := selector.
	oldIntervals := self keywordsIntervals.
	oldArguments := arguments.
	self
		arguments: varNodeCollection;
		selector: newSelector.
	self changeSourceSelector: oldSelector keywordsIntervals: oldIntervals arguments: oldArguments
]

{ #category : 'replacing' }
ASTMethodNode >> replaceNode: aNode withNode: anotherNode [
	aNode == body ifTrue: [self body: anotherNode].
	self arguments: (arguments
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing' }
ASTMethodNode >> scope [
	^ scope
]

{ #category : 'accessing' }
ASTMethodNode >> scope: aScopedNode [
	scope := aScopedNode
]

{ #category : 'accessing' }
ASTMethodNode >> selector [
	^ selector
]

{ #category : 'accessing' }
ASTMethodNode >> selector: aSelector [
	keywordsPositions := nil.
	selector := aSelector asSymbol
]

{ #category : 'initialization' }
ASTMethodNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	self
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList
]

{ #category : 'accessing' }
ASTMethodNode >> selectorAndArgumentNames [
	"Returns the selector and argument names portion of a method as a string"

	^ self arguments
		ifEmpty: [self keywords first]
		ifNotEmpty: [| lastArgument |
			lastArgument := self arguments last.
			self source first: lastArgument start + (lastArgument name size - 1)]
]

{ #category : 'accessing' }
ASTMethodNode >> selectorParts [
	^ self keywords collect: [:each | each asSymbol]
]

{ #category : 'accessing compiled method' }
ASTMethodNode >> semanticScope: aSemanticScope [
	compilationContext ifNil: [
		compilationContext := aSemanticScope targetClass compiler compilationContext].

	self compilationContext semanticScope: aSemanticScope
]

{ #category : 'accessing' }
ASTMethodNode >> source [
	^source
]

{ #category : 'accessing' }
ASTMethodNode >> source: anObject [
	source := anObject
]

{ #category : 'accessing' }
ASTMethodNode >> sourceCode [
	"compatibility to MethodNode"
	^source
]

{ #category : 'accessing' }
ASTMethodNode >> start [
	^ 1
]

{ #category : 'accessing' }
ASTMethodNode >> statements [
	^ self body statements
]

{ #category : 'accessing' }
ASTMethodNode >> statements: aCollection [
	self body statements: aCollection
]

{ #category : 'accessing' }
ASTMethodNode >> stop [
	^(self sourceCode ifNotNil: [:src | src] ifNil: [ self formattedCode ]) size
]

{ #category : 'accessing' }
ASTMethodNode >> temporaries [
	^ self body temporaries
]

{ #category : 'accessing' }
ASTMethodNode >> temporaryNames [
	^ self body temporaryNames
]

{ #category : 'tree accessing' }
ASTMethodNode >> variableWith: aSymbol [

	| matcher block |
	matcher := ASTParseTreeSearcher new.
	block := [ :aNode :answer | aNode ].

	matcher
		matches: aSymbol do: block;
		matchesArgument: aSymbol do: block.
	^ matcher executeTree: self initialAnswer: nil
]
