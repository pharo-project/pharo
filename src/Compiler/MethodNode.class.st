"
I am the root of the parse tree.
"
Class {
	#name : #MethodNode,
	#superclass : #ParseNode,
	#instVars : [
		'selectorOrFalse',
		'precedence',
		'arguments',
		'block',
		'primitive',
		'encoder',
		'temporaries',
		'properties',
		'sourceText',
		'locationCounter',
		'localsPool'
	],
	#category : 'Compiler-ParseNodes'
}

{ #category : #visiting }
MethodNode >> accept: aVisitor [
	^aVisitor visitMethodNode: self
]

{ #category : #'code generation (closures)' }
MethodNode >> addLocalsToPool: locals [ "<Set of: TempVariableNode>"
	localsPool ifNil:
		[localsPool := IdentitySet new].
	localsPool addAll: locals
]

{ #category : #accessing }
MethodNode >> arguments [
	"For transformations etc, not used in compilation"
	^arguments
]

{ #category : #accessing }
MethodNode >> arguments: aSequence [
	"For transformations etc, not used in compilation"
	arguments := aSequence
]

{ #category : #initialize-release }
MethodNode >> block [
	^ block
]

{ #category : #'debugger support' }
MethodNode >> blockExtentsToTempsMap [
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := Parser new
						encoderClass: encoder class;
						parse: self sourceCode
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]
]

{ #category : #accessing }
MethodNode >> body [
	^block
]

{ #category : #converting }
MethodNode >> decompileString [ 
	"Answer a string description of the parse tree whose root is the receiver."

	^self formattedCode

]

{ #category : #'code generation' }
MethodNode >> encoder [
	^ encoder
]

{ #category : #'code generation (closures)' }
MethodNode >> ensureClosureAnalysisDone [
	block blockExtent ifNil:
		[temporaries := block analyseArguments: arguments temporaries: temporaries rootNode: self]
]

{ #category : #printing }
MethodNode >> formattedCode [
	^ self fullPrintString 
]

{ #category : #'code generation' }
MethodNode >> generate [
	"The receiver is the root of a parse tree. Answer a CompiledMethod."

	^self generate: CompiledMethodTrailer empty
]

{ #category : #'code generation' }
MethodNode >> generate: trailer [ 
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	^self generate: trailer using: CompiledMethod
]

{ #category : #'code generation' }
MethodNode >> generate: trailer ifQuick: methodBlock [
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]]) ifFalse:
		[^self].
	v := block code.
	v < 0 ifTrue:
		[^self].
	v = LdSelf ifTrue:
		[^methodBlock value: (CompiledMethod toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3) ifTrue:
		[^methodBlock value: (CompiledMethod toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType)) ifTrue:
		[^methodBlock value: (CompiledMethod toReturnField: v trailerBytes: trailer)].
	v // 256 = 1 ifTrue:
		[^methodBlock value: (CompiledMethod toReturnField: v \\ 256 trailerBytes: trailer)]
]

{ #category : #'as yet unclassified' }
MethodNode >> generate: trailer using: aCompiledMethodClass [
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits locals literals stack header method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	self ensureClosureAnalysisDone.
	encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primitive > 0
					ifTrue: [encoder sizeCallPrimitive: primitive]
					ifFalse: [0])
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	locals := arguments, temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	encoder noteBlockExtent: block blockExtent hasLocals: locals.
	header := encoder computeMethodHeaderForNumArgs: arguments size
					numTemps: locals size
					numLits: (nLits := (literals := encoder allLiterals) size)
					primitive: primitive.
	method := trailer
					createMethod: blkSize
					class: aCompiledMethodClass
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primitive > 0 ifTrue:
		[encoder genCallPrimitive: primitive.
		 primErrNode ifNotNil:
			[primErrNode emitCodeForStore: stack encoder: encoder]].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method
]

{ #category : #'as yet unclassified' }
MethodNode >> generate: trailer using: aCompiledMethodClass ifQuick: methodBlock [
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]]) ifFalse:
		[^self].
	v := block code.
	v < 0 ifTrue:
		[^self].
	v = LdSelf ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType)) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1 ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]
]

{ #category : #'code generation' }
MethodNode >> generateWithSource [
	"Answer a CompiledMethod with source encoded in trailer."
	
	| source |
	
	"for doits, we need to store the source pretty printed from the 
	AST to get the return and methodName correct"
	self selector isDoIt ifTrue: [sourceText := self printString].
	
	^self generate: (CompiledMethodTrailer new sourceCode: sourceText).
]

{ #category : #'debugger support' }
MethodNode >> hasGeneratedMethod [
	^encoder hasGeneratedMethod
]

{ #category : #'code generation (closures)' }
MethodNode >> locationCounter [
	^locationCounter
]

{ #category : #printing }
MethodNode >> methodClass [

	^ encoder classEncoding
]

{ #category : #'code generation (closures)' }
MethodNode >> noteBlockEntry: aBlock [
	"Evaluate aBlock with the numbering for the block entry."
	locationCounter ifNil:
		[locationCounter := -1].
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2
]

{ #category : #'code generation (closures)' }
MethodNode >> noteBlockExit: aBlock [
	"Evaluate aBlock with the numbering for the block exit."
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2
]

{ #category : #converting }
MethodNode >> preen [
	"Preen for pretty-printing and/or decompilation.
	 i.e. post-process to cover up for inadequacies in both algorithms.
	 Currently one case, hiding the assignment to the arg of an inlined block arg to ifNotNil:,
		(var := expr) ifNil: [...] ifNotNil: [...]    =>    expr ifNil: [...] ifNotNil: [:var| ...]."

	self preenLocalIfNotNilArg
]

{ #category : #converting }
MethodNode >> preenLocalIfNotNilArg [
	"Try and spot a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block
	 and convert it to expr ifNil: [...] ifNotNil: [:var| ...].  Deal both with the pretty-print case where
	 the block already declares the variable and the decompile case where it does not."

	| varsToHide |
	varsToHide := Set new.
	self nodesDo:
		[:node| | variable |
		(node isMessageNode
		and: [node macroPrinter == #printIfNilNotNil:indent:
		and: [node receiver isMessageNode
		and: [node receiver selector key == #==
		and: [node receiver receiver isAssignmentNode
		and: [(variable := node receiver receiver variable) isTemp
		and: [variable isRemote not
		and: [variable isOnlySubnodeOf: node in: self]]]]]]]) ifTrue:
			[node arguments last arguments isEmpty
				ifTrue: [node arguments last arguments: { variable }.
						varsToHide add: variable]
				ifFalse: [self assert: node arguments last arguments asArray =  { variable }].
			 node receiver receiver: node receiver receiver value]].
	varsToHide notEmpty ifTrue:
		[self nodesDo:
			[:node|
			((node == self or: [node isBlockNode])
			and: [node temporaries anySatisfy: [:temp| varsToHide includes: temp]]) ifTrue:
				[node temporaries: (node temporaries reject: [:temp| varsToHide includes: temp])]]]
]

{ #category : #accessing }
MethodNode >> primitiveErrorVariableName [
	"Answer the primitive error code temp name, or nil if none."
	(primitive isInteger and: [primitive > 0]) ifTrue:
		[properties pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil

	"(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: Parser) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' error: foo module: ''bar''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' module: ''bar'' error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate"
]

{ #category : #printing }
MethodNode >> printOn: aStream [
	| selectorNode |
	selectorNode := self selectorNode.
	precedence = 1
		ifTrue:
			[selectorNode isForFFICall
				ifTrue: [selectorNode
							printAsFFICallWithArguments: arguments
							on: aStream
							indent: 0]
				ifFalse: [aStream nextPutAll: selectorNode key]]
		ifFalse:
			[selectorNode key keywords with: arguments do:
				[:kwd :arg |
				aStream nextPutAll: kwd; space; nextPutAll: arg key; space]].
	comment == nil ifFalse:
		[aStream crtab: 1.
		 self printCommentOn: aStream indent: 1].
	block printTemporaries: temporaries on: aStream doPrior: [aStream crtab: 1].
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream crtab: 1.
			 self printPrimitiveOn: aStream]].
	self printPropertiesOn: aStream.
	self printPragmasOn: aStream.
	aStream crtab: 1.
	block printStatementsOn: aStream indent: 0
]

{ #category : #printing }
MethodNode >> printPragmasOn: aStream [
	properties ifNil: [^self].
	properties pragmas do:
		[:pragma|
		"Primitives are printed in printPrimitiveOn:; skip these"
		(Parser primitivePragmaSelectors includes: pragma keyword) ifFalse:
			[aStream crtab: 1.
			 pragma printOn: aStream]]
]

{ #category : #printing }
MethodNode >> printPrimitiveOn: aStream [
	"Print the primitive on aStream"

	| primDecl |
	primitive = 0
		ifTrue: [ ^ self ].
	primitive = 120
		ifTrue: [ 
			"External call spec"
			^ aStream print: encoder literals first ].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue: [ 
			primDecl := encoder literals at: 1.
			(primDecl at: 2) asString printOn: aStream.
			(primDecl at: 1)
				ifNotNil: [ :moduleName | 
					aStream nextPutAll: ' module: '.
					moduleName asString printOn: aStream ] ]
		ifFalse: [ aStream print: primitive ].
	self primitiveErrorVariableName
		ifNotNil: [ :primitiveErrorVariableName | 
			aStream
				nextPutAll: ' error: ';
				nextPutAll: primitiveErrorVariableName ].
	aStream nextPut: $>.
]

{ #category : #printing }
MethodNode >> printPropertiesOn: aStream [
	properties ifNil: [^self].
	properties propertyKeysAndValuesDo:
		[:prop :val|
		aStream crtab; nextPut: $<.
		prop = #on:in:
			ifTrue:
				[prop keywords with: val do:
					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]
			ifFalse:
				[prop = #on
					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 
					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 
		aStream nextPut: $>]
]

{ #category : #'code generation' }
MethodNode >> properties [
	^ properties
]

{ #category : #'source mapping' }
MethodNode >> rawSourceRanges [

	^self rawSourceRangesAndMethodDo: [:rawSourceRanges :method| rawSourceRanges]
]

{ #category : #'source mapping' }
MethodNode >> rawSourceRangesAndMethodDo: aBinaryBlock [
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
	methNode := Parser new
					encoderClass: encoder class;
					parse: self sourceCode
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method
]

{ #category : #'code generation (closures)' }
MethodNode >> referencedValuesWithinBlockExtent: anInterval [ 
	^(localsPool select:
		[:temp|
		 temp isReferencedWithinBlockExtent: anInterval]) collect:
			[:temp|
			temp isRemote ifTrue: [temp remoteNode] ifFalse: [temp]]
]

{ #category : #'primitive error codes' }
MethodNode >> removeAndRenameLastTempIfErrorCode [
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 temporaries last
			name: primitiveErrorVariableName
			key: primitiveErrorVariableName
			code: temporaries last code.
		 temporaries removeLast].
]

{ #category : #accessing }
MethodNode >> removeProperty: aSymbol [
	properties := properties copyWithout: (Association
											key: aSymbol
											value: (properties propertyValueAt: aSymbol))
]

{ #category : #'code generation' }
MethodNode >> selector [ 
	"Answer the message selector for the method represented by the receiver."

	(selectorOrFalse isSymbol)
		ifTrue: [^selectorOrFalse].
	^selectorOrFalse key.

]

{ #category : #initialize-release }
MethodNode >> selector: symbol [

	selectorOrFalse := symbol
]

{ #category : #initialize-release }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim [ 
	"Initialize the receiver with respect to the arguments given."

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim
]

{ #category : #initialize-release }
MethodNode >> selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict [
	"Initialize the receiver with respect to the arguments given."

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim.
	properties := propDict.
]

{ #category : #'code generation' }
MethodNode >> selectorNode [
	"Answer a SelectorNode for the message selector of the method represented by the receiver."

	^(selectorOrFalse isMemberOf: SelectorNode)
		ifTrue: [selectorOrFalse]
		ifFalse: [SelectorNode new key: selectorOrFalse]
]

{ #category : #initialize-release }
MethodNode >> source: stringOrText [

	sourceText := stringOrText
]

{ #category : #printing }
MethodNode >> sourceCode [

	^ self sourceText.
]

{ #category : #printing }
MethodNode >> sourceText [

	^ sourceText ifNil: [self printString]
]

{ #category : #initialize-release }
MethodNode >> sourceText: stringOrText [

	sourceText := stringOrText
]

{ #category : #printing }
MethodNode >> tempNames [
	^ encoder tempNames
]

{ #category : #accessing }
MethodNode >> temporaries [
	"For transformations etc, not used in compilation"
	^temporaries
]

{ #category : #accessing }
MethodNode >> temporaries: aSequence [
	"For transformations etc, not used in compilation"
	temporaries := aSequence
]
