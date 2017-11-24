"
I represent a bracketed block with 0 or more arguments and 1 or more statements. If I am initialized with no statements, I create one. I have a flag to tell whether my last statement returns a value from the enclosing method. My last three fields remember data needed for code generation. I can emit for value in the usual way, in which case I create a literal method (actually a context remotely copied) to be evaluated by sending it value: at run time. Or I can emit code to be evaluated in line; this only happens at the top level of a method and in conditionals and while-loops, none of which have arguments.
"
Class {
	#name : #BlockNode,
	#superclass : #ParseNode,
	#instVars : [
		'arguments',
		'statements',
		'returns',
		'nArgsNode',
		'size',
		'remoteCopyNode',
		'temporaries',
		'optimized',
		'optimizedMessageNode',
		'actualScopeIfOptimized',
		'blockExtent',
		'remoteTempNode',
		'copiedValues',
		'closureCreationNode',
		'startOfLastStatement'
	],
	#category : #'Compiler-ParseNodes'
}

{ #category : #'instance creation' }
BlockNode class >> statements: statements returns: returns [
	^ self new statements: statements returns: returns
]

{ #category : #'instance creation' }
BlockNode class >> withJust: aNode [
	^ self new statements: (Array with: aNode) returns: false
]

{ #category : #visiting }
BlockNode >> accept: aVisitor [
	^aVisitor visitBlockNode: self
]

{ #category : #'code generation (closures)' }
BlockNode >> actualScope [
	"Answer the actual scope for the receiver.  If this is an unoptimized block then it is its
	 actual scope, but if this is an optimized block then the actual scope is some outer block."
	^actualScopeIfOptimized ifNil: [self]
]

{ #category : #accessing }
BlockNode >> addArgument: aTempVariableNode [
	temporaries := temporaries copyWith: aTempVariableNode
]

{ #category : #'code generation (closures)' }
BlockNode >> addHoistedTemps: additionalTemporaries [ "<SequenceableCollection>"
	additionalTemporaries do:
		[:temp|
		temp definingScope ifNil:
			[temp definingScope: self]].
	temporaries := (temporaries isNil or: [temporaries isEmpty])
					ifTrue: [additionalTemporaries copy]
					ifFalse:
						[temporaries last isIndirectTempVector
							ifTrue: [temporaries allButLast, additionalTemporaries, { temporaries last }]
							ifFalse: [temporaries, additionalTemporaries]]
]

{ #category : #'code generation (closures)' }
BlockNode >> addRemoteTemp: aTempVariableNode rootNode: rootNode [ "<MethodNode>"
	"Add aTempVariableNode to my actualScope's sequence of
	 remote temps.  If I am an optimized block then the actual
	 scope is my actualScopeIfOptimized, otherwise it is myself."
	remoteTempNode == nil ifTrue:
		[remoteTempNode := RemoteTempVectorNode new
								name: self remoteTempNodeName
								index: arguments size + temporaries size
								type: LdTempType
								scope: 0.
		 actualScopeIfOptimized
			ifNil:
				[self addTempNode: remoteTempNode.
				 remoteTempNode definingScope: self]
			ifNotNil: [actualScopeIfOptimized addHoistedTemps: { remoteTempNode }]].
	remoteTempNode addRemoteTemp: aTempVariableNode encoder: rootNode encoder.
	"use remove:ifAbsent: because the deferred analysis for optimized
	 loops can result in the temp has already been hoised into the root."
	self removeTempNode: aTempVariableNode ifAbsent: [
		self actualScope removeTempNode: aTempVariableNode ifAbsent: ["should not happen"]].
	^remoteTempNode
]

{ #category : #'code generation (closures)' }
BlockNode >> addTempNode: aTempVariableNode [
	"Utilities for when we want to add some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries add: aTempVariableNode
]

{ #category : #'code generation (closures)' }
BlockNode >> analyseArguments: methodArguments temporaries: methodTemporaries rootNode: rootNode [ "<MethodNode>" "^<Sequence of: <TempVarNade>>"
	"Top level entry-point for analysing temps within the hierarchy of blocks in the receiver's method.
	 Answer the (possibly modified) sequence of temp vars.
	 Need to hoist temps out of macro-optimized blocks into their actual blocks.
	 Need to note reads and writes to temps from blocks other than their actual blocks to determine
	 whether blocks can be local (simple slots within a block/method context) or remote (slots in
	 indirection vectors that are shared between contexts by sharing indirection vectors).

	 The algorithm is based on numbering temporary reads and writes and block extents.
	 The index used for numbering starts at zero and is incremented on every block entry
	 and block exit.  So the following
		| a b blk r1 r2 t |
		a := 1. b := 2. t := 0.
		blk := [ | s | s := a + b. t := t + s].
		r1 := blk value.
		b := -100.
		r2 := blk value.
		r1 -> r2 -> t
	is numbered as
		method block 0 to: 6:
		| a b blk r1 r2 t |
		a w@1 := 1. b w@1 := 2. t w@1 := 0.
		blk w@5 := [entry@2 | s |
					 t  w@3 := t r@3 + a r@3 + b r@3
					] exit@4.
		r1 w@5 := blk r@5 value.
		b w@5 := nil.
		r2 w@5 := blk r@5 value.
		r1 r@5 -> r2 r@5 -> t r@5
	So:
		b and blk cannot be copied because for both there exists a write @5 that follows a
			read @4 within block 2 through 4
		t must be remote because there exists a write @3 within block (2 to: 4)
	Complications are introduced by optimized blocks.  In the following temp is written to
	after it is closed over by [ temp ] since the inlined block is executed more than once.
		| temp coll |
		coll := OrderedCollection new.
		1 to: 5 do: [ :index | 
			temp := index. 
			coll add: [ temp ] ].
		[(coll collect: [:ea| ea value]) asArray = #(5 5 5 5 5)] assert.
	In the following i is local to the block and must be initialized each time around the loop
	but if the block is inlined it must be declared at method level.
		| col |
		col := OrderedCollection new.
		1 to: 3 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
		[(col collect: [ :each | each value ]) asArray = #(2 3 4)] assert"
	[arguments isEmpty or: [arguments hasEqualElements: methodArguments]] assert.
	arguments := methodArguments asArray. "won't change"
	[temporaries isNil or: [temporaries isEmpty or: [temporaries hasEqualElements: methodTemporaries]]] assert.
	temporaries := OrderedCollection withAll: methodTemporaries.

	[optimized not] assert. "the top-level block should not be optimized."
	self analyseTempsWithin: self rootNode: rootNode assignmentPools: Dictionary new.

	"The top-level block needs to reindex temporaries since analysis may have rearranged them.
	 This happens when temps are made remote and/or a remote node is added."
	temporaries withIndexDo:
		[:temp :offsetPlusOne| temp index: arguments size + offsetPlusOne - 1].

	"Answer the (possibly modified) sequence of temps."
	^temporaries asArray
]

{ #category : #'code generation (closures)' }
BlockNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	| effectiveScope blockStart |
	effectiveScope := optimized
						ifTrue: [actualScopeIfOptimized := scopeBlock]
						ifFalse: [self].

	arguments ifNotNil:
		[arguments do: [:temp| temp definingScope: self]].
	temporaries ifNotNil:
		[temporaries do: [:temp| temp definingScope: self]].

	optimized ifFalse: "if optimized this isn't an actual scope"
		[rootNode noteBlockEntry:
			[:entryNumber|
			 blockExtent := (blockStart := entryNumber) to: 0]].

	"Need to enumerate a copy because closure analysis can add a statement
	 via ifHasRemoteTempNodeEnsureInitializationStatementExists:."
	statements copy do:
		[:statement|
		 statement analyseTempsWithin: effectiveScope rootNode: rootNode assignmentPools: assignmentPools].

	optimized
		ifTrue: "if optimized loop need to add nils for any temps read before written"
			[optimizedMessageNode isOptimizedLoop ifTrue:
				[self nilReadBeforeWrittenTemps]]
		ifFalse: "if optimized this isn't an actual scope"
			[rootNode noteBlockExit:
				[:exitNumber|
				 blockExtent := blockStart to: exitNumber]].

	"Now that the analysis is done move any temps that need to be moved."
	self postNumberingProcessTempsWithin: effectiveScope rootNode: rootNode.

	"This is simply a nicety for compiler developers..."
	temporaries do:
		[:temp|
		(temp isIndirectTempVector and: [temp name includes: $?]) ifTrue:
			[temp name: temp definingScope remoteTempNodeName]]
]

{ #category : #accessing }
BlockNode >> arguments [
	^arguments ifNil: [#()]
]

{ #category : #accessing }
BlockNode >> arguments: argNodes [ 
	"Decompile."

	arguments := argNodes
]

{ #category : #'initialize-release' }
BlockNode >> arguments: argNodes statements: statementsCollection returns: returnBool from: encoder [
	"Compile."

	arguments := argNodes.
	statements := statementsCollection size > 0
				ifTrue: [statementsCollection]
				ifFalse: [argNodes size > 0
						ifTrue: [statementsCollection copyWith: arguments last]
						ifFalse: [Array with: NodeNil]].
	optimized := false.
	returns := returnBool
]

{ #category : #accessing }
BlockNode >> block [
	^ self
]

{ #category : #'code generation (closures)' }
BlockNode >> blockExtent [ "^<Interval>"
	^blockExtent
]

{ #category : #accessing }
BlockNode >> closureCreationNode [
	closureCreationNode ifNil:
		[closureCreationNode := LeafNode new
									key: #closureCreationNode
									code: nil].
	^closureCreationNode
]

{ #category : #'code generation' }
BlockNode >> code [

	^statements first code
]

{ #category : #'code generation (closures)' }
BlockNode >> computeCopiedValues: rootNode [
	| referencedValues |
	referencedValues := rootNode referencedValuesWithinBlockExtent: blockExtent.
	^(referencedValues reject: [:temp| temp isDefinedWithinBlockExtent: blockExtent])
		asArray sort: ParseNode tempSortBlock
]

{ #category : #'code generation (closures)' }
BlockNode >> constructClosureCreationNode: encoder [
	copiedValues := self computeCopiedValues: encoder rootNode.
	encoder supportsClosureOpcodes ifTrue:
		[^self closureCreationNode].
	"Without the bytecode we can still get by."
	^MessageNode new
		receiver: (encoder encodeVariable: 'thisContext')
		selector: #closureCopy:copiedValues:
		arguments: (Array
						with: (encoder encodeLiteral: arguments size)
						with: (copiedValues isEmpty
								ifTrue: [NodeNil]
								ifFalse: [BraceNode new elements: copiedValues]))
		precedence: 3
		from: encoder
]

{ #category : #printing }
BlockNode >> decompileString [ 
	"Answer a string description of the parse tree whose root is the receiver."

	^ self printString

]

{ #category : #'code generation (closures)' }
BlockNode >> deoptimize [
	optimized := false.
	optimizedMessageNode := nil
]

{ #category : #'code generation' }
BlockNode >> emitCodeExceptLast: stack encoder: encoder [
	| position nextToLast |
	position := stack position.
	nextToLast := statements size - 1.
	1 to: nextToLast do:
		[:i | | statement |
		statement := statements at: i.
		statement emitCodeForEffect: stack encoder: encoder.
		[stack position = position] assert].
]

{ #category : #'code generation (closures)' }
BlockNode >> emitCodeForEvaluatedClosureValue: stack encoder: encoder [
	| position |
	position := stack position.
	stack position: arguments size + copiedValues size.
	temporaries size timesRepeat:
		[NodeNil emitCodeForValue: stack encoder: encoder].
	self
		reindexingLocalsDo: [self emitCodeForEvaluatedValue: stack encoder: encoder]
		encoder: encoder.
	self returns ifFalse:
		[encoder genReturnTopToCaller.
		 pc := encoder methodStreamPosition].
	stack position: position
]

{ #category : #'code generation' }
BlockNode >> emitCodeForEvaluatedEffect: stack encoder: encoder [
	| position |
	position := stack position.
	self returns
		ifTrue: 
			[self emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1]
		ifFalse: 
			[self emitCodeExceptLast: stack encoder: encoder.
			statements last emitCodeForEffect: stack encoder: encoder].
	[stack position = position] assert
]

{ #category : #'code generation' }
BlockNode >> emitCodeForEvaluatedValue: stack encoder: encoder [
	| position |
	position := stack position.
	self emitCodeExceptLast: stack encoder: encoder.
	statements last emitCodeForBlockValue: stack encoder: encoder.
	[stack position - 1 = position] assert
]

{ #category : #'code generation (closures)' }
BlockNode >> emitCodeForValue: stack encoder: encoder [
	"if not supportsClosureOpcodes closureCreationSupportNode is the
	 node for thisContext closureCopy: numArgs [ copiedValues: { values } ]"
	encoder supportsClosureOpcodes
		ifTrue:
			[copiedValues do:
				[:copiedValue| copiedValue emitCodeForValue: stack encoder: encoder].
			 closureCreationNode pc: encoder methodStreamPosition + 1.
			 encoder
				genPushClosureCopyNumCopiedValues: copiedValues size
				numArgs: arguments size
				jumpSize: size.
			 stack
				pop: copiedValues size;
				push: 1]
		ifFalse:
			[closureCreationNode emitCodeForValue: stack encoder: encoder.
			 encoder genJumpLong: size]. "Force a two byte jump."
	"Emit the body of the block"
	self emitCodeForEvaluatedClosureValue: stack encoder: encoder
]

{ #category : #accessing }
BlockNode >> firstArgument [
	^ arguments first
]

{ #category : #'code generation (closures)' }
BlockNode >> ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode [
	"If a remoteTempNode has been added ensure a statement exists to initialize it."
	remoteTempNode ~~ nil ifTrue:
		[(statements notEmpty
		  and: [statements first isAssignmentNode
		  and: [statements first variable isTemp
		  and: [statements first variable isIndirectTempVector]]])
			ifTrue: "If this is a decompiled tree, or if a temporary has been added later in
					the analysis then there already is a temp vector initialization node."
				[(statements first variable ~~ remoteTempNode) ifTrue:
					[statements first variable become: remoteTempNode].
				 statements first value numElements: remoteTempNode remoteTemps size]
			ifFalse:
				[statements addFirst: (remoteTempNode nodeToInitialize: rootNode encoder)]].
]

{ #category : #testing }
BlockNode >> isBlockNode [
	^true
]

{ #category : #testing }
BlockNode >> isComplex [

	^statements size > 1 or: [statements size = 1 and: [statements first isComplex]]
]

{ #category : #testing }
BlockNode >> isJust: node [

	returns ifTrue: [^false].
	^statements size = 1 and: [statements first == node]
]

{ #category : #testing }
BlockNode >> isJustCaseError [

	^ statements size = 1 and:
		[statements first
			isMessage: #caseError
			receiver: [:r | r==NodeSelf]
			arguments: nil]
]

{ #category : #testing }
BlockNode >> isQuick [
	^ statements size = 1
		and: [statements first isVariableReference
				or: [statements first isSpecialConstant]]
]

{ #category : #'code generation (closures)' }
BlockNode >> makeTemporariesRemovable [
	"Utilities for when we want to remove some temporaries."
	
	temporaries isArray ifTrue:
		[temporaries := temporaries asOrderedCollection].
]

{ #category : #accessing }
BlockNode >> nArgsSlot [
	"Private for the Encoder to use in bindArg"
	^nArgsNode
]

{ #category : #accessing }
BlockNode >> nArgsSlot: anInteger [
	"Private for the Encoder to use in bindArg"
	nArgsNode := anInteger
]

{ #category : #'code generation (closures)' }
BlockNode >> nilReadBeforeWrittenTemps [
	| visitor readBeforeWritten |
	self accept: (visitor := OptimizedBlockLocalTempReadBeforeWrittenVisitor new).
	readBeforeWritten := visitor readBeforeWritten.
	temporaries reverseDo:
		[:temp|
		((readBeforeWritten includes: temp)
		 and: [temp isRemote not]) ifTrue:
			[statements addFirst: (AssignmentNode new variable: temp value: NodeNil)]]
]

{ #category : #'code generation (closures)' }
BlockNode >> noteOptimized [
	optimized := true
]

{ #category : #'code generation (closures)' }
BlockNode >> noteOptimizedIn: anOptimizedMessageNode [
	optimized := true.
	optimizedMessageNode := anOptimizedMessageNode
]

{ #category : #'initialize-release' }
BlockNode >> noteSourceRangeStart: start end: end encoder: encoder [
	"Note two source ranges for this node.  One is for the debugger
	 and is of the last expression, the result of the block.  One is for
	 source analysis and is for the entire block."
	encoder
		noteSourceRange: (start to: end)
		forNode: self closureCreationNode.
	startOfLastStatement
		ifNil:
			[encoder
				noteSourceRange: (start to: end)
				forNode: self]
		ifNotNil:
			[encoder
				noteSourceRange: (startOfLastStatement to: end - 1)
				forNode: self]
]

{ #category : #accessing }
BlockNode >> numberOfArguments [

	^arguments size
]

{ #category : #accessing }
BlockNode >> optimized [
	^optimized
]

{ #category : #'code generation (closures)' }
BlockNode >> optimizedBlockHoistTempsInto: scopeBlock [ "<BlockNode>"
	"This is a No-op for all nodes except non-optimized BlockNodes."
	"Let's assume the special > 0 guard in MessageNode>>analyseTempsWithin:forValue:encoder: is correct.
	 Then we can simply hoist our temps up."
	[arguments isNil or: [arguments size <= 1]] assert.
	(arguments notNil and: [arguments notEmpty]) ifTrue:
		[scopeBlock addHoistedTemps: arguments.
		arguments := #()].
	temporaries notEmpty ifTrue:
		[scopeBlock addHoistedTemps: temporaries.
		temporaries := #()]
]

{ #category : #'code generation (closures)' }
BlockNode >> postNumberingProcessTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode [ "<MethodNode>"
	"A temp can be local (and copied) if it is not written to after it is captured.
	 A temp cannot be local if it is written to remotely.
	 Need to enumerate a copy of the temporaries because any temps becoming remote
	 will be removed from temporaries in analyseClosure: (and a single remote temp node
	 will get added)"
	temporaries copy do:
		[:each|
		each isIndirectTempVector ifFalse:
			[each analyseClosure: rootNode]].

	"If this is an optimized node we need to hoist temporaries up into the relevant block scope."
	optimized ifTrue:
		[self optimizedBlockHoistTempsInto: scopeBlock].

	"Now we may have added a remoteTempNode.  So we need a statement to initialize it."
	self ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode.

	"Now add all arguments and locals to the pool so that copiedValues can be computed during sizing."
	rootNode
		addLocalsToPool: arguments;
		addLocalsToPool: temporaries
]

{ #category : #printing }
BlockNode >> printArgumentsOn: aStream indent: level [
	arguments size = 0 ifTrue: [^ self].
	arguments do:
		[:arg | aStream nextPut: $:;  nextPutAll: arg key;  space].
	aStream nextPut: $|; space.
	"If >0 args and >1 statement, put all statements on separate lines"
	statements size > 1 ifTrue:
		[aStream crtab: level]
]

{ #category : #printing }
BlockNode >> printOn: aStream indent: level [

	"statements size <= 1 ifFalse: [aStream crtab: level]."
	aStream nextPut: $[.
	self printArgumentsOn: aStream indent: level.
	(self printTemporaries: temporaries on: aStream doPrior: []) ifTrue:
		["If >0 temps and >1 statement, put all statements on separate lines"
		 statements size > 1
			ifTrue: [aStream crtab: level]
			ifFalse: [aStream space]].
	self printStatementsOn: aStream indent: level.
	aStream nextPut: $]
]

{ #category : #printing }
BlockNode >> printStatementsOn: aStream indent: levelOrZero [
	| len shown thisStatement level |
	level := 1 max: levelOrZero.
	comment == nil
		ifFalse: 
			[self printCommentOn: aStream indent: level.
			aStream crtab: level].
	len := shown := statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [shown := 1 max: shown - 1]
		ifFalse: ["should a trailing nil be printed or not? Not if it is an implicit result."
				(arguments size = 0
				and: [len >= 1
				and: [(statements at: len) == NodeNil
				and: [len = 1
					or: [len > 1
						and: [(statements at: len - 1) isMessageNode
						and: [(statements at: len - 1) isNilIf]]]]]])
					ifTrue: [shown := shown - 1]].
	1 to: shown do: 
		[:i | 
		thisStatement := statements at: i.
		thisStatement printOn: aStream indent: level.
		i < shown ifTrue: [aStream nextPut: $.; crtab: level].
		(thisStatement comment ~~ nil and: [thisStatement comment size > 0])
			ifTrue: 
				[i = shown ifTrue: [aStream crtab: level].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [aStream crtab: level]]]
]

{ #category : #printing }
BlockNode >> printTemporaries: tempSequence on: aStream doPrior: aBlock [
	"Print any in-scope temporaries.  If there are any evaluate aBlock
	 prior to printing.  Answer whether any temporaries were printed."
	| tempStream seen |
	tempSequence ifNil:
		[^false].
	tempStream := (String new: 16) writeStream.
	"This is for the decompiler which canmot work out which optimized block a particular temp is
	 local to and hence may produce diplicates as in
		expr ifTrue: [| aTemp | ...] ifFalse: [| aTemp | ...]"
	seen := Set new.
	tempSequence do:
		[:tempNode |
		tempNode isIndirectTempVector
			ifTrue:
				[tempNode remoteTemps do:
					[:tempVariableNode|
					 (tempVariableNode scope >= 0
					  and: [(seen includes: tempNode key) not]) ifTrue:
						[tempStream space; nextPutAll: (seen add: tempVariableNode key)]]]
			ifFalse:
				[(tempNode scope >= -1
				  and: ["This is for the decompiler which may create a block arg when converting
						a while into a to:do: but won't remove it form temporaries"
					   tempNode isBlockArg not
				  and: [(seen includes: tempNode key) not]]) ifTrue:
					[tempStream space; nextPutAll: (seen add: tempNode key)]]].
	tempStream position = 0 ifTrue:
		[^false].
	aBlock value.
	aStream nextPut: $|; nextPutAll: tempStream contents; space; nextPut: $|.
	^true
]

{ #category : #'code generation (closures)' }
BlockNode >> reindexingLocalsDo: aBlock encoder: encoderOrNil [
	"Evaluate aBlock wih arguments, temporaries and copiedValues reindexed for
	 their positions within the receiver's block, restoring the correct indices afterwards.
	 If encoder is not nil remember the temps for this block's extent."
	| tempIndices result tempsToReindex |
	[copiedValues notNil] assert.
	tempsToReindex := arguments asArray, copiedValues, temporaries.
	tempIndices := tempsToReindex collect: [:temp| temp index].
	tempsToReindex withIndexDo:
		[:temp :newIndex| temp index: newIndex - 1. [temp index + 1 = newIndex] assert].
	encoderOrNil ifNotNil:
		[encoderOrNil noteBlockExtent: blockExtent hasLocals: tempsToReindex].
	result := aBlock ensure:
				["Horribly pragmatic hack.  The copiedValues will have completely
				  unrelated indices within the closure method and sub-method.
				  Avoiding the effort of rebinding temps in the inner scope simply
				  update the indices to their correct ones during the generation of
				  the closure method and restore the indices immedately there-after."
				 tempsToReindex with: tempIndices do:
					[:temp :oldIndex| temp index: oldIndex. [temp index = oldIndex] assert]].
	^result
]

{ #category : #'code generation (closures)' }
BlockNode >> remoteTempNodeName [
	"Answer a useful name for a RemoteTempVectorNode in the receiver."
	| prefix scope extent |
	prefix := actualScopeIfOptimized ifNil: ['<'] ifNotNil: [ '<...'].
	scope := self.
	[extent := scope blockExtent.
	 extent == nil
	 and: [scope actualScope ~~ scope]] whileTrue:
		[scope := scope actualScope].
	^extent
		ifNil: [prefix, '?-?>']
		ifNotNil:
			[prefix, extent first printString, '-',
				(extent last isZero
					ifTrue: ['?']
					ifFalse: [extent last printString]), '>']
]

{ #category : #'code generation (closures)' }
BlockNode >> removeTempNode: aTempVariableNode ifAbsent: aBlock [
	"Utilities for when we want to remove some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries remove: aTempVariableNode ifAbsent: aBlock
	
]

{ #category : #accessing }
BlockNode >> returnLast [

	self returns
		ifFalse: 
			[returns := true.
			statements at: statements size put: statements last asReturnNode]
]

{ #category : #accessing }
BlockNode >> returnNilIfNoOther [

	self returns
		ifFalse: 
			[statements last == NodeNil ifFalse: [statements add: NodeNil].
			self returnLast]
]

{ #category : #accessing }
BlockNode >> returnSelfIfNoOther: encoder [

	self returns ifTrue:[^self].
	statements last == NodeSelf ifFalse: [
		statements := statements copyWith: (encoder encodeVariable: 'self').
	].
	self returnLast.

]

{ #category : #testing }
BlockNode >> returns [

	^returns or: [statements last isReturningIf]
]

{ #category : #'code generation' }
BlockNode >> sizeCodeExceptLast: encoder [
	| codeSize |
	codeSize := 0.
	1 to: statements size - 1 do: 
		[:i | | statement |
		 statement := statements at: i.
		 codeSize := codeSize + (statement sizeCodeForEffect: encoder)].
	^codeSize
]

{ #category : #'code generation (closures)' }
BlockNode >> sizeCodeForEvaluatedClosureValue: encoder [
	"The closure value primitives push the arguments and the copied values.
	 The compiler guarantees that any copied values come before all local temps.
	 So on closure activation we only need to push nils for the remaining temporaries."
	^temporaries size * (NodeNil sizeCodeForValue: encoder)
	+ (self
		reindexingLocalsDo: [self sizeCodeForEvaluatedValue: encoder]
		encoder: nil "don't store temps yet")
	+ (self returns ifTrue: [0] ifFalse: [encoder sizeReturnTopToCaller])
]

{ #category : #'code generation' }
BlockNode >> sizeCodeForEvaluatedEffect: encoder [

	^self returns
		ifTrue: [self sizeCodeForEvaluatedValue: encoder]
		ifFalse: [(self sizeCodeExceptLast: encoder)
				+ (statements last sizeCodeForEffect: encoder)]
]

{ #category : #'code generation' }
BlockNode >> sizeCodeForEvaluatedValue: encoder [

	^(self sizeCodeExceptLast: encoder)
		+ (statements last sizeCodeForBlockValue: encoder)
]

{ #category : #'code generation (closures)' }
BlockNode >> sizeCodeForValue: encoder [
	"Compute the size for the creation of the block and its code."
	"If we have the closure bytecodes constructClosureCreationNode: will note
	 the copied values in the copiedValues inst var and answer #pushCopiedValues."
	closureCreationNode := self constructClosureCreationNode: encoder.
	"Remember size of body for emit time so we know the size of the jump around it."
	size := self sizeCodeForEvaluatedClosureValue: encoder.
	^encoder supportsClosureOpcodes
		ifTrue:
			[(copiedValues inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
			 + (encoder sizePushClosureCopyNumCopiedValues: copiedValues size numArgs: arguments size jumpSize: size)
			 + size]
		ifFalse:
			["closureCreationSupportNode is send closureCopy:copiedValues:"
			(closureCreationNode sizeCodeForValue: encoder)
			 + (encoder sizeJumpLong: size)
			 + size]
]

{ #category : #accessing }
BlockNode >> startOfLastStatement [
	^startOfLastStatement
]

{ #category : #accessing }
BlockNode >> startOfLastStatement: anInteger [
	"Note the source index of the start of the last full statement.  The
	 last full statement is the value answered by a block and hence the
	 expression the debugger should display as the value of the block."
	startOfLastStatement := anInteger
]

{ #category : #'equation translation' }
BlockNode >> statements [
	^statements
]

{ #category : #'equation translation' }
BlockNode >> statements: val [
	statements := val
]

{ #category : #'initialize-release' }
BlockNode >> statements: statementsCollection returns: returnBool [ 
	"Decompile."

	| returnLast |
	returnLast := returnBool.
	returns := false.
	statements := 
		(statementsCollection size > 1 
			and: [(statementsCollection at: statementsCollection size - 1) 
					isReturningIf])
				ifTrue: 
					[returnLast := false.
					statementsCollection allButLast]
				ifFalse: [statementsCollection size = 0
						ifTrue: [Array with: NodeNil]
						ifFalse: [statementsCollection]].
	arguments := #().
	temporaries := #().
	optimized := false.
	returnLast ifTrue: [self returnLast]
]

{ #category : #accessing }
BlockNode >> temporaries [
	^temporaries ifNil: [#()]
]

{ #category : #accessing }
BlockNode >> temporaries: aCollection [
	temporaries := aCollection
]
