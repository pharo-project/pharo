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
	#name : 'OCMethodNode',
	#superclass : 'OCProgramNode',
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
OCMethodNode class >> selector: aSymbol [
	^self selector: aSymbol arguments: #()
]

{ #category : 'instance creation' }
OCMethodNode class >> selector: aSymbol arguments: valueNodes [
	^(self new)
		selector: aSymbol;
		arguments: valueNodes;
		yourself
]

{ #category : 'instance creation' }
OCMethodNode class >> selector: aSymbol arguments: variableNodes body: aSequenceNode [
	^ self new 
		arguments: variableNodes;
		selector: aSymbol;
		body: aSequenceNode;
		yourself
]

{ #category : 'instance creation' }
OCMethodNode class >> selector: aSymbol body: aSequenceNode [
	^self
		selector: aSymbol
		arguments: #()
		body: aSequenceNode
]

{ #category : 'instance creation' }
OCMethodNode class >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	^(self new)
		selector: aSymbol
			keywordsPositions: positionList
			arguments: valueNodes;
		yourself
]

{ #category : 'comparing' }
OCMethodNode >> = anObject [
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
OCMethodNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitMethodNode: self
]

{ #category : 'adding-removing' }
OCMethodNode >> addNode: aNode [
	^ body addNode: aNode
]

{ #category : 'adding-removing' }
OCMethodNode >> addPragma: aPragmaNode [

	pragmas := pragmas copyWith: aPragmaNode.
	aPragmaNode parent: self
]

{ #category : 'replacing' }
OCMethodNode >> addReplacement: aStringReplacement [
	replacements ifNil: [^self].
	replacements add: aStringReplacement
]

{ #category : 'adding-removing' }
OCMethodNode >> addReturn [
	^ body addReturn
]

{ #category : 'adding-removing' }
OCMethodNode >> addSelfReturn [
	^ body addSelfReturn
]

{ #category : 'accessing' }
OCMethodNode >> allArgumentVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allArgumentVariables;
		yourself
]

{ #category : 'accessing' }
OCMethodNode >> allDefinedVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : 'iterating' }
OCMethodNode >> allSequenceNodes [

	^ self allChildren select: #isSequence
]

{ #category : 'accessing' }
OCMethodNode >> allStatements [
	"return the statements including variable definition."

	^ (OrderedCollection withAll: self temporaries)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
OCMethodNode >> argumentNames [
	^ self arguments collect: [:each | each name] as: Array
]

{ #category : 'accessing' }
OCMethodNode >> arguments [
	^arguments
]

{ #category : 'accessing' }
OCMethodNode >> arguments: variableNodes [
	arguments := variableNodes.
	arguments do: [:each | each parent: self]
]

{ #category : 'converting' }
OCMethodNode >> asSequenceNode [
	^body
]

{ #category : 'accessing' }
OCMethodNode >> body [
	^body
]

{ #category : 'accessing' }
OCMethodNode >> body: stmtsNode [
	body := stmtsNode.
	body parent: self
]

{ #category : 'private - replacing' }
OCMethodNode >> changeSourceSelector: oldSelector keywordsIntervals: oldIntervals arguments: oldArguments [
	"If this is the same number of arguments, we try a one to one replacement of selector parts and arguments. If this is not the c
ase try to rewrite the signature as a whole, what unfortunately drops the comments within the signature."

	[ (oldIntervals size = self keywordsPositions size and: [ oldArguments size = arguments size ])
		ifTrue: [
			self selectorParts keysAndValuesDo: [ :index :part |
				self addReplacement: (OCStringReplacement
					replaceFrom: (oldIntervals at: index) first
					to: (oldIntervals at: index) last
					with: part) ].
			oldArguments with: arguments do: [ :old :new |
				self addReplacement: (OCStringReplacement
					replaceFrom: old start
					to: old stop
					with: new name) ] ]
		ifFalse: [
			self addReplacement: (OCStringReplacement
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
OCMethodNode >> children [
	^ OrderedCollection new
		addAll: arguments;
		addAll: pragmas;
		add: body;
		yourself
]

{ #category : 'replacing' }
OCMethodNode >> clearReplacements [
	replacements := nil
]

{ #category : 'accessing - compiled method' }
OCMethodNode >> compilationContext [
	^ compilationContext
]

{ #category : 'accessing - compiled method' }
OCMethodNode >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : 'accessing - conceptual' }
OCMethodNode >> conceptualArgumentSize [
	"Return the cumulted length of the parameters (yes parameters are called arguments in Pharo - not good!). It does not count spaces and the selectors.
	Note that this length is based on the tree structure and not the source code. It does not reflect the selection interval but the space the selector would take if displayed on a single line without any space."

	^ self arguments
		inject: 0
		into: [ :sum :each | sum + each name size ]
]

{ #category : 'accessing - conceptual' }
OCMethodNode >> conceptualSelectorSize [
	"Return the length of the selector. It does not take into length of parameters nor the spaces.
	Note that this length is based on the tree structure and not the source code. It does not reflect the selection interval but the space the selector would take if displayed on a single line without any space."

	^ selector size
]

{ #category : 'accessing - conceptual' }
OCMethodNode >> conceptualSignatureSize [
	"Return the full length of the method signature (called with a strange name in Pharo: the patterned selector. Note that this length is based on the tree structure and not the source code. It does not reflect the selection interval but the space the signature would take if displayed on a single line."

	"When we count foo: a b: b, we add three extras spaces so args size * 2 - 1"

	^ self conceptualSelectorSize + self conceptualArgumentSize
		+ (self arguments
		ifNotEmpty: [ self arguments size * 2 - 1 ]
		ifEmpty: [ 0 ])
]

{ #category : 'matching' }
OCMethodNode >> copyInContext: aDictionary [
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
OCMethodNode >> defines: aName [
	^ (arguments anySatisfy: [ :each | each name = aName ])
		or: [ self pragmas anySatisfy: [ :pragma | pragma defines: aName ] ]
]

{ #category : 'comparing' }
OCMethodNode >> equalTo: anObject withMapping: aDictionary [
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
OCMethodNode >> extractSubtreeWith: aSourceCode [
	| extractedTree |
	extractedTree := self parserClass
		parseExpression: aSourceCode
		onError: [ :string :pos | ^ nil ].
	extractedTree ifNil: [ ^ nil ].
	(extractedTree isSequence and: [ extractedTree statements isEmpty ])
		ifTrue: [ ^ nil ].
	extractedTree := extractedTree isSequence
		ifTrue:
			[ OCParseTreeSearcher treeMatchingStatements: aSourceCode in: self ]
		ifFalse:
			[ OCParseTreeSearcher treeMatching: aSourceCode in: self ].	"or: [aTree isReturn]"
	^ extractedTree
		"ifNotNil: [ extractedTree parent: (self parentOfSubtree: extractedTree) ]"
]

{ #category : 'accessing' }
OCMethodNode >> firstPrecodeComment [
	self comments ifEmpty: [ ^nil ].
	^ self comments first contents
]

{ #category : 'testing' }
OCMethodNode >> hasArgumentNamed: aString [
	^ self arguments anySatisfy: [ :argument| argument name = aString ]
]

{ #category : 'testing' }
OCMethodNode >> hasArguments [

	^ arguments isNotEmpty
]

{ #category : 'testing' }
OCMethodNode >> hasBlock [

	^ body hasBlock 
]

{ #category : 'testing' }
OCMethodNode >> hasPragmaNamed: aSymbol [
	self pragmaNamed: aSymbol ifAbsent: [ ^ false ].
	^ true
]

{ #category : 'testing' }
OCMethodNode >> hasTemporaries [
	^ self temporaries isNotEmpty
]

{ #category : 'testing' }
OCMethodNode >> hasTemporaryNamed: aString [
	^ self temporaries anySatisfy: [ :temp| temp name = aString ]
]

{ #category : 'comparing' }
OCMethodNode >> hash [
	^ ((self selector hash bitXor: (self hashForCollection: self arguments)) bitXor: (self hashForCollection: self pragmas)) bitXor: self body hash
]

{ #category : 'initialization' }
OCMethodNode >> initialize [
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
OCMethodNode >> isDoIt [
	^false
]

{ #category : 'errors' }
OCMethodNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	(self arguments anySatisfy: [:each | each isFaulty]) ifTrue:[ ^true].
	(self pragmas anySatisfy: [:each | each isFaulty]) ifTrue:[ ^true].
	^self body isFaulty
]

{ #category : 'testing' }
OCMethodNode >> isLast: aNode [
	^body isLast: aNode
]

{ #category : 'testing' }
OCMethodNode >> isMethod [
	^true
]

{ #category : 'testing' }
OCMethodNode >> isPrimitive [
	^ self pragmas anySatisfy: [ :each | each isPrimitive ]
]

{ #category : 'testing' }
OCMethodNode >> isUsingAsReturnValue: aNode [
	^body == aNode and: [aNode lastIsReturn]
]

{ #category : 'accessing' }
OCMethodNode >> keywords [
	^ selector keywords
]

{ #category : 'accessing' }
OCMethodNode >> keywordsIntervals [
	^selector keywords
		with: self keywordsPositions
		collect: [:keyword :start| start to: (start = 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ]
]

{ #category : 'accessing' }
OCMethodNode >> keywordsPositions [
	^keywordsPositions ifNil: [ (selector keywords collect: [:char| 0 ]) asIntegerArray ]
]

{ #category : 'accessing' }
OCMethodNode >> keywordsPositions: aPositionsList [
	keywordsPositions := aPositionsList ifNotNil: [:list| list asIntegerArray ]
]

{ #category : 'testing' }
OCMethodNode >> lastIsReturn [
	^body lastIsReturn
]

{ #category : 'replacing' }
OCMethodNode >> map: oldNode to: newNode [
	nodeReplacements at: oldNode put: newNode
]

{ #category : 'replacing' }
OCMethodNode >> mappingFor: oldNode [
	^nodeReplacements at: oldNode ifAbsent: [oldNode]
]

{ #category : 'matching' }
OCMethodNode >> match: aNode inContext: aDictionary [
	self class = aNode class ifFalse: [ ^ false ].
	aDictionary at: '-source-' put: aNode source.
	self selector = aNode selector ifFalse: [ ^ false ].
	^ (self matchList: arguments against: aNode arguments inContext: aDictionary)
		and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
		and: [ body match: aNode body inContext: aDictionary ] ]
]

{ #category : 'matching' }
OCMethodNode >> matchPragmas: matchNodes against: pragmaNodes inContext: aDictionary [
	matchNodes isEmpty ifTrue: [
		aDictionary at: '-pragmas-' put: pragmaNodes.
		^ true ].
	^ matchNodes allSatisfy: [ :matchNode |
		pragmaNodes anySatisfy: [ :pragmaNode |
			matchNode match: pragmaNode inContext: aDictionary ] ]
]

{ #category : 'accessing' }
OCMethodNode >> methodNode [
	^self
]

{ #category : 'accessing' }
OCMethodNode >> methodOrBlockNode [
	^ self
]

{ #category : 'accessing' }
OCMethodNode >> newSource [
	replacements ifNil: [^self formattedCode].
	^[self reformatSource] on: Error do: [:ex | ex return: self formattedCode]
]

{ #category : 'accessing' }
OCMethodNode >> numArgs [
	^self selector numArgs
]

{ #category : 'testing' }
OCMethodNode >> offsetIsComment: anOffset [
	"check is the offset in the source part of a comment"

	^ self allComments
		anySatisfy: [ :comment | anOffset between: comment start and: comment stop ]
]

{ #category : 'copying' }
OCMethodNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self pragmas: (self pragmas collect: [ :each | each copy ]).
	self body: self body copy
]

{ #category : 'testing' }
OCMethodNode >> pragmaNamed: aSymbol [
	^ self pragmaNamed: aSymbol ifAbsent: [ KeyNotFound signalFor: aSymbol  ]
]

{ #category : 'testing' }
OCMethodNode >> pragmaNamed: aSymbol ifAbsent: absentBlock [
	^ self pragmas
		detect: [ :pragma| pragma selector = aSymbol ]
		ifNone: absentBlock
]

{ #category : 'testing' }
OCMethodNode >> pragmaNamed: aSymbol ifPresent: foundBlock [
	^ self pragmas
		detect: [ :pragma| pragma selector = aSymbol ]
		ifFound: foundBlock
]

{ #category : 'testing' }
OCMethodNode >> pragmaNamed: aSymbol ifPresent: presentBlock ifAbsent: absentBlock [
	^ self pragmas
		detect: [ :pragma| pragma selector = aSymbol ]
		ifFound: presentBlock
		ifNone: absentBlock
]

{ #category : 'accessing' }
OCMethodNode >> pragmas [
	^ pragmas
]

{ #category : 'accessing' }
OCMethodNode >> pragmas: aCollection [
	pragmas := aCollection.
	pragmas do: [ :each | each parent: self ]
]

{ #category : 'printing' }
OCMethodNode >> printOn: aStream [
	aStream nextPutAll: self formattedCode
]

{ #category : 'testing' }
OCMethodNode >> references: aVariableName [
	^body references: aVariableName
]

{ #category : 'private' }
OCMethodNode >> reformatSource [
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
OCMethodNode >> removePragma: aPragmaNode [

	pragmas := pragmas copyWithout: aPragmaNode
]

{ #category : 'adding-removing' }
OCMethodNode >> removePragmaNamed: aPragmaName [

	self pragmaNamed: aPragmaName ifPresent: [ :pragma | self removePragma: pragma ]
]

{ #category : 'adding-removing' }
OCMethodNode >> removeSubtree: aTree [

	^ aTree isReturn
		ifTrue: [ OCParseTreeRewriter
					 replace: aTree formattedCode
					 with: '' in: self
					 onInterval: aTree sourceInterval ]
		ifFalse: [ OCParseTreeRewriter
					  replaceStatements: aTree formattedCode
		 			  with: '' in: self
					  onInterval: aTree sourceInterval ]
]

{ #category : 'accessing' }
OCMethodNode >> renameSelector: newSelector andArguments: varNodeCollection [
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
OCMethodNode >> replaceNode: aNode withNode: anotherNode [
	aNode == body ifTrue: [self body: anotherNode].
	self arguments: (arguments
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing' }
OCMethodNode >> scope [
	^ scope
]

{ #category : 'accessing' }
OCMethodNode >> scope: aScopedNode [
	scope := aScopedNode
]

{ #category : 'accessing' }
OCMethodNode >> selector [
	^ selector
]

{ #category : 'accessing' }
OCMethodNode >> selector: aSelector [
	keywordsPositions := nil.
	selector := aSelector asSymbol
]

{ #category : 'initialization' }
OCMethodNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	self
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList
]

{ #category : 'accessing' }
OCMethodNode >> selectorAndArgumentNames [
	"Returns the selector and argument names portion of a method as a string"

	^ self arguments
		ifEmpty: [self keywords first]
		ifNotEmpty: [| lastArgument |
			lastArgument := self arguments last.
			self source first: lastArgument start + (lastArgument name size - 1)]
]

{ #category : 'accessing' }
OCMethodNode >> selectorParts [
	^ self keywords collect: [:each | each asSymbol]
]

{ #category : 'accessing compiled method' }
OCMethodNode >> semanticScope: aSemanticScope [
	compilationContext ifNil: [
		compilationContext := aSemanticScope targetClass compiler compilationContext].

	self compilationContext semanticScope: aSemanticScope
]

{ #category : 'accessing' }
OCMethodNode >> source [
	^source
]

{ #category : 'accessing' }
OCMethodNode >> source: anObject [
	source := anObject
]

{ #category : 'accessing' }
OCMethodNode >> sourceCode [
	"compatibility to MethodNode"
	^source
]

{ #category : 'accessing' }
OCMethodNode >> start [
	^ 1
]

{ #category : 'accessing' }
OCMethodNode >> statements [
	^ self body statements
]

{ #category : 'accessing' }
OCMethodNode >> statements: aCollection [
	self body statements: aCollection
]

{ #category : 'accessing' }
OCMethodNode >> stop [
	^(self sourceCode ifNotNil: [:src | src] ifNil: [ self formattedCode ]) size
]

{ #category : 'accessing' }
OCMethodNode >> temporaries [
	^ self body temporaries
]

{ #category : 'accessing' }
OCMethodNode >> temporaryNames [
	^ self body temporaryNames
]

{ #category : 'tree accessing' }
OCMethodNode >> variableWith: aSymbol [

	| matcher block |
	matcher := OCParseTreeSearcher new.
	block := [ :aNode :answer | aNode ].

	matcher
		matches: aSymbol do: block;
		matchesArgument: aSymbol do: block.
	^ matcher executeTree: self initialAnswer: nil
]
