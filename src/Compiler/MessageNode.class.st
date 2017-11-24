"
I represent a receiver and its message.
	
Precedence codes:
	1 unary
	2 binary
	3 keyword
	4 other
	
If special>0, I compile special code in-line instead of sending messages with literal methods as remotely copied contexts.
"
Class {
	#name : #MessageNode,
	#superclass : #ParseNode,
	#instVars : [
		'receiver',
		'selector',
		'precedence',
		'special',
		'arguments',
		'sizes',
		'equalNode',
		'caseErrorNode',
		'originalSelector',
		'originalArguments'
	],
	#classVars : [
		'MacroEmitters',
		'MacroPrinters',
		'MacroSelectors',
		'MacroSizers',
		'MacroTransformers',
		'StdTypers',
		'ThenFlag'
	],
	#category : #'Compiler-ParseNodes'
}

{ #category : #'class initialization' }
MessageNode class >> initialize [
	"MessageNode initialize"
	MacroSelectors := 
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:
			repeat ).
	MacroTransformers := 
		#(	transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:
			transformAnd: transformOr:
			transformWhile: transformWhile: transformWhile: transformWhile:
			transformToDo: transformToDo:
			transformCase: transformCase:
			transformIfNil: transformIfNil:  transformIfNilIfNotNil: transformIfNotNilIfNil:
			transformRepeat: ).
	MacroEmitters := 
		#(	emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForToDo:encoder:value: emitCodeForToDo:encoder:value:
			emitCodeForCase:encoder:value: emitCodeForCase:encoder:value:
			emitCodeForIfNil:encoder:value: emitCodeForIfNil:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForRepeat:encoder:value:).
	MacroSizers := 
		#(	sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value:
			sizeCodeForToDo:value: sizeCodeForToDo:value:
			sizeCodeForCase:value: sizeCodeForCase:value:
			sizeCodeForIfNil:value: sizeCodeForIfNil:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForRepeat:value:).
	MacroPrinters := 
		#(	printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:
			printIfOn:indent: printIfOn:indent:
			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:
			printToDoOn:indent: printToDoOn:indent:
			printCaseOn:indent: printCaseOn:indent:
			printIfNil:indent: printIfNil:indent: printIfNilNotNil:indent: printIfNilNotNil:indent:
			printRepeatOn:indent:)
]

{ #category : #visiting }
MessageNode >> accept: aVisitor [
	^aVisitor visitMessageNode: self
]

{ #category : #'code generation (closures)' }
MessageNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	"Assignments within optimized loops are tricky.  Because a loop repeats a
	 write to a temporary in an optimized loop effectively occurs after the loop.
	 To handle this collect the set of temps assigned to in optimized loops and
	 add extra writes after traversing the optimized loop constituents."
	| writtenToTemps |
	self isOptimizedLoop ifTrue:
		[{ receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[assignmentPools at: node put: Set new]]].
	"receiver is nil in cascades"
	receiver == nil ifFalse:
		[receiver analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools].
	arguments do:
		[:node|
		node == nil ifFalse: "last argument of optimized to:do: can be nil"
			[node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]].
	"Add assignments representing subsequent iterations
	 and redo the closure analysis for the written-to temps."
	self isOptimizedLoop ifTrue:
		[writtenToTemps := Set new.
		 { receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[(assignmentPools removeKey: node) do:
					[:temp|
					temp isBlockArg ifFalse: "ignore added assignments to to:do: loop args"
						[writtenToTemps add: temp.
						 temp addWriteWithin: node at: rootNode locationCounter]]]].
		 writtenToTemps isEmpty ifFalse:
			[(writtenToTemps asSortedCollection: ParseNode tempSortBlock) do:
				[:each| each analyseClosure: rootNode].
			 (writtenToTemps collect: [:each| each definingScope]) do:
				[:blockNode|
				blockNode ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode]]]
]

{ #category : #'equation translation' }
MessageNode >> arguments [
	^arguments
]

{ #category : #'equation translation' }
MessageNode >> arguments: list [
	arguments := list
]

{ #category : #visiting }
MessageNode >> argumentsInEvaluationOrder [
	"Answer the receivers arguments in evaluation order.
	 If the receiver is a transformed to:do: node this will undo the misordering done by the transformation."
	^(special > 0
	   and: [(MacroTransformers at: special) == #transformToDo:
	   and: [arguments size >= 7]])
		"arguments are in a weid order and may be nil in a transformed to:do: loop.  sigh...
		 c.f. emitCodeForToDo:encoder:value:"
		ifTrue:
			[(arguments at: 7)	"limitInit"
				ifNil: [{	(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]
				ifNotNil: [:limitInit|
						{ limitInit.
						(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]]
		ifFalse:
			[arguments]
]

{ #category : #testing }
MessageNode >> canCascade [

	^receiver ~~ NodeSuper
]

{ #category : #cascading }
MessageNode >> cascadeReceiver [
	"Nil out rcvr (to indicate cascade) and return what it had been."

	| rcvr |
	rcvr := receiver.
	receiver := nil.
	^rcvr
]

{ #category : #private }
MessageNode >> checkBlock: node as: nodeName from: encoder [

	^self checkBlock: node as: nodeName from: encoder maxArgs: 0
]

{ #category : #private }
MessageNode >> checkBlock: node as: nodeName from: encoder maxArgs: maxArgs [
	"Answer true if node is a BlockNode with at most maxArgs arguments.
	This check is required in order to inline some special messages.
	Notify some undue usage of these special messages."

	node isBlockNode ifFalse: [ ^false ].
	node numberOfArguments <= maxArgs ifTrue: [ ^true ].
	^encoder notify: '<- ', nodeName , ' of ' , (MacroSelectors at: special) , ' has too many arguments'
]

{ #category : #'code generation' }
MessageNode >> emitCodeForCase: stack encoder: encoder value: forValue [

	| braceNode sizeStream allReturn |
	forValue ifFalse:
		[^super emitCodeForEffect: stack encoder: encoder].
	braceNode := arguments first.
	sizeStream := sizes readStream.
	receiver emitCodeForValue: stack encoder: encoder.
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last | | thenSize elseSize |
		thenSize := sizeStream next.
		elseSize := sizeStream next.
		last ifFalse: [encoder genDup. stack push: 1].
		keyNode emitCodeForEvaluatedValue: stack encoder: encoder.
		equalNode emitCode: stack args: 1 encoder: encoder.
		self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
		last ifFalse: [encoder genPop. stack pop: 1].
		valueNode emitCodeForEvaluatedValue: stack encoder: encoder.
		last ifTrue: [stack pop: 1].
		valueNode returns ifFalse:
			[self emitCodeForJump: elseSize encoder: encoder.
			 allReturn := false].
		(last and: [allReturn]) ifTrue:
			[self emitCodeForJump: elseSize encoder: encoder]].
	arguments size = 2
		ifTrue:
			[arguments last emitCodeForEvaluatedValue: stack encoder: encoder] "otherwise: [...]"
		ifFalse:
			[NodeSelf emitCodeForValue: stack encoder: encoder.
			caseErrorNode emitCode: stack args: 0 encoder: encoder]
]

{ #category : #'code generation' }
MessageNode >> emitCodeForEffect: stack encoder: encoder [
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: false]
		ifFalse: 
			[super emitCodeForEffect: stack encoder: encoder]
]

{ #category : #'code generation' }
MessageNode >> emitCodeForIf: stack encoder: encoder value: forValue [
	| thenExpr thenSize elseExpr elseSize |
	thenSize := sizes at: 1.
	elseSize := sizes at: 2.
	(forValue not and: [elseSize * thenSize > 0]) ifTrue:
		"Two-armed IFs forEffect share a single pop"
		[^super emitCodeForEffect: stack encoder: encoder].
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
			pc := encoder methodStreamPosition.
			thenExpr emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1.  "then and else alternate; they don't accumulate"
			thenExpr returns not ifTrue:
				"...not ifTrue: avoids using ifFalse: alone during this compile)"
				"Elide jump over else after a return"
				[self emitCodeForJump: elseSize encoder: encoder].
			elseExpr emitCodeForEvaluatedValue: stack encoder: encoder]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[thenSize > 0
				ifTrue:
					[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					thenExpr emitCodeForEvaluatedEffect: stack encoder: encoder]
				ifFalse:
					[self emitCodeForBranchOn: true dist: elseSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					elseExpr emitCodeForEvaluatedEffect: stack encoder: encoder]]
]

{ #category : #'code generation' }
MessageNode >> emitCodeForIfNil: stack encoder: encoder value: forValue [

	| theNode theSize ifNotNilSelector |
	theNode := arguments first.
	theSize := sizes at: 1.
	ifNotNilSelector := #ifNotNil:.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue ifTrue: [encoder genDup. stack push: 1].
	encoder genPushSpecialLiteral: nil. stack push: 1.
	equalNode emitCode: stack args: 1 encoder: encoder.
	self 
		emitCodeForBranchOn: (selector key == ifNotNilSelector)
		dist: theSize 
		pop: stack 
		encoder: encoder.
	pc := encoder methodStreamPosition.
	forValue 
		ifTrue: 
			[encoder genPop. stack pop: 1.
			theNode emitCodeForEvaluatedValue: stack encoder: encoder]	
		ifFalse: [theNode emitCodeForEvaluatedEffect: stack encoder: encoder]
]

{ #category : #'code generation' }
MessageNode >> emitCodeForRepeat: stack encoder: encoder value: forValue [ 
	" L1: ... Jmp(L1)"
	| loopSize |
	loopSize := sizes at: 1.
	receiver emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]
]

{ #category : #'code generation' }
MessageNode >> emitCodeForToDo: stack encoder: encoder value: forValue [ 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt limitInit test block incStmt blockSize |
	initStmt := arguments at: 4.
	limitInit := arguments at: 7.
	test := arguments at: 5.
	block := arguments at: 3.
	incStmt := arguments at: 6.
	blockSize := sizes at: 1.
	loopSize := sizes at: 2.
	limitInit == nil
		ifFalse: [limitInit emitCodeForEffect: stack encoder: encoder].
		
	"This will return the receiver of to:do: which is the initial value of the loop"
	forValue
		ifTrue: [initStmt emitCodeForValue: stack encoder: encoder.]
		ifFalse: [initStmt emitCodeForEffect: stack encoder: encoder].
	test emitCodeForValue: stack encoder: encoder.
	self emitCodeForBranchOn: false dist: blockSize pop: stack encoder: encoder.
	pc := encoder methodStreamPosition.
	block emitCodeForEvaluatedEffect: stack encoder: encoder.
	incStmt emitCodeForEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
]

{ #category : #'code generation' }
MessageNode >> emitCodeForValue: stack encoder: encoder [
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: true]
		ifFalse: 
			[receiver ~~ nil ifTrue: [receiver emitCodeForValue: stack encoder: encoder].
			arguments do: [:argument | argument emitCodeForValue: stack encoder: encoder].
			pc := encoder methodStreamPosition + 1. "debug pc is first byte of the send, i.e. the next byte".
			selector
				emitCode: stack
				args: arguments size
				encoder: encoder
				super: receiver == NodeSuper]
]

{ #category : #'code generation' }
MessageNode >> emitCodeForWhile: stack encoder: encoder value: forValue [
        "L1: ... Bfp(L2)|Btp(L2) ... Jmp(L1) L2: "
        | cond theArguments stmt stmtSize loopSize |
        cond := receiver.
        theArguments := arguments ifEmpty: [ Array with: (BlockNode withJust: NodeNil) ].
        stmt := theArguments at: 1.
        stmtSize := sizes at: 1.
        loopSize := sizes at: 2.
        cond emitCodeForEvaluatedValue: stack encoder: encoder.
        self emitCodeForBranchOn: ((selector key == #whileFalse:) or: [ selector key == #whileFalse ]) "Bfp for whileTrue"
                                        dist: stmtSize pop: stack encoder: encoder.   "Btp for whileFalse"
        pc := encoder methodStreamPosition.
        stmt emitCodeForEvaluatedEffect: stack encoder: encoder.
        self emitCodeForJump: 0 - loopSize encoder: encoder.
        forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]
]

{ #category : #testing }
MessageNode >> ensureCanCascade: encoder [

	special > 0 ifTrue:
		[special := 0.
		selector := encoder encodeSelector: originalSelector.
		arguments := originalArguments.
		receiver isBlockNode ifTrue: [receiver deoptimize].
		arguments do:
			[:each|
			each isBlockNode ifTrue:
				[each deoptimize]]]
]

{ #category : #private }
MessageNode >> ifNilReceiver [

	^receiver
]

{ #category : #testing }
MessageNode >> isComplex [
	
	^(special between: 1 and: 10) or: [arguments size > 2 or: [receiver isComplex]]
]

{ #category : #testing }
MessageNode >> isMessage [
	^true
]

{ #category : #testing }
MessageNode >> isMessage: selSymbol receiver: rcvrPred arguments: argsPred [
	"Answer whether selector is selSymbol, and the predicates rcvrPred and argsPred
	 evaluate to true with respect to receiver and the list of arguments.  If selSymbol or
	 either predicate is nil, it means 'don't care'.  Note that argsPred takes numArgs
	 arguments.  All block arguments are ParseNodes."

	^(selSymbol isNil or: [selSymbol==selector key]) and:
		[(rcvrPred isNil or: [rcvrPred value: receiver]) and:
			[(argsPred isNil or: [argsPred valueWithArguments: arguments])]]
]

{ #category : #testing }
MessageNode >> isMessageNode [
	^true
]

{ #category : #testing }
MessageNode >> isNilIf [

	^(special between: 3 and: 4)
	   and: [(arguments first returns or: [arguments first isJust: NodeNil])
	   and: [(arguments last returns or: [arguments last isJust: NodeNil])]]
]

{ #category : #testing }
MessageNode >> isOptimized [
	^special > 0
]

{ #category : #testing }
MessageNode >> isOptimizedLoop [
	^special > 0
	   and: [#(transformWhile: transformToDo:) includes: (MacroTransformers at: special)]
]

{ #category : #testing }
MessageNode >> isReturningIf [

	^((special between: 3 and: 4) "ifTrue:ifFalse:/ifFalse:ifTrue:"
	    or: [special between: 17 and: 18]) "ifNil:ifNotNil:/ifNotNil:ifNil:"
		and: [arguments first returns and: [arguments last returns]]
]

{ #category : #printing }
MessageNode >> macroPrinter [

	special > 0 ifTrue: [^MacroPrinters at: special].
	^nil

]

{ #category : #'macro transformations' }
MessageNode >> noteSpecialSelector: selectorSymbol [
	"special > 0 denotes specially treated (potentially inlined) messages. "

	special := MacroSelectors indexOf: selectorSymbol.

]

{ #category : #printing }
MessageNode >> precedence [

	^precedence
]

{ #category : #printing }
MessageNode >> printCaseOn: aStream indent: level [ 
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"
	| braceNode otherwise extra |
	braceNode := arguments first.
	otherwise := arguments last.
	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:
		[otherwise := nil].
	receiver
		printOn: aStream
		indent: level
		precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printOn: aStream indent: level]
		ifFalse: 
			[aStream nextPutAll: '{'; crtab: level + 1.
			 braceNode casesForwardDo:
				[:keyNode :valueNode :last | 
				keyNode printOn: aStream indent: level + 1.
				aStream nextPutAll: ' -> '.
				valueNode isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						extra := 1]
					ifFalse: [extra := 0].
				valueNode printOn: aStream indent: level + 1 + extra.
				last ifTrue: [aStream nextPut: $}]
					ifFalse: [aStream nextPut: $.;
							 crtab: level + 1]]].
	otherwise notNil ifTrue:
		[aStream crtab: level + 1; nextPutAll: ' otherwise: '.
		 extra := otherwise isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						 1]
					ifFalse: [0].
		 otherwise printOn: aStream indent: level + 1 + extra]
]

{ #category : #printing }
MessageNode >> printIfNil: aStream indent: level [

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array with: arguments first)
		on: aStream indent: level
]

{ #category : #printing }
MessageNode >> printIfNilNotNil: aStream indent: level [

	self printReceiver: receiver ifNilReceiver on: aStream indent: level.

	(arguments first isJust: NodeNil) ifTrue:
		[^ self printKeywords: #ifNotNil:
				arguments: { arguments second }
				on: aStream indent: level].
	(arguments second isJust: NodeNil) ifTrue:
		[^ self printKeywords: #ifNil:
				arguments: { arguments first }
				on: aStream indent: level].
	^ self printKeywords: #ifNil:ifNotNil:
			arguments: arguments
			on: aStream indent: level
]

{ #category : #printing }
MessageNode >> printIfOn: aStream indent: level [

	receiver ifNotNil:
		[receiver printOn: aStream indent: level + 1 precedence: precedence].
	(arguments last isJust: NodeNil) ifTrue:
		[^self printKeywords: #ifTrue: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments last isJust: NodeFalse) ifTrue:
		[^self printKeywords: #and: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments first isJust: NodeNil) ifTrue:
		[^self printKeywords: #ifFalse: arguments: (Array with: arguments last)
					on: aStream indent: level].
	(arguments first isJust: NodeTrue) ifTrue:
		[^self printKeywords: #or: arguments: (Array with: arguments last)
					on: aStream indent: level].
	self printKeywords: #ifTrue:ifFalse: arguments: arguments
					on: aStream indent: level
]

{ #category : #printing }
MessageNode >> printKeywords: key arguments: args on: aStream indent: level [
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue:
		[aStream space; nextPutAll: key.
		 ^self].
	keywords := key keywords.
	doCrTab := args size > 2
				or: [{receiver} , args anySatisfy:
						[:thisArg |
						thisArg notNil
						and: [thisArg isBlockNode
							 or: [thisArg isMessageNode and: [thisArg precedence >= 3]]]]].
	1 to: (args size min: keywords size) do:
		[:i |
		arg := args at: i.
		kwd := keywords at: i.
		doCrTab
			ifTrue: [aStream crtab: level+1. indent := 1] "newline after big args"
			ifFalse: [aStream space. indent := 0].
		aStream nextPutAll: kwd; space.
		arg printOn: aStream
			indent: level + 1 + indent
			precedence: (precedence = 2 ifTrue: [1] ifFalse: [precedence])]
]

{ #category : #printing }
MessageNode >> printOn: aStream indent: level [
	"may not need this check anymore - may be fixed by the #receiver: change"
	special ifNil: [^aStream nextPutAll: '** MessageNode with nil special **'].

	special > 0 ifTrue:
		[^self perform: self macroPrinter with: aStream with: level].

	self printReceiver: receiver on: aStream indent: level.
	selector isForFFICall
		ifTrue:
			[aStream space.
			 selector
				printAsFFICallWithArguments: arguments
				on: aStream
				indent: 0]
		ifFalse:
			[self printKeywords: selector key
				 arguments: arguments
				 on: aStream
				 indent: level]
]

{ #category : #printing }
MessageNode >> printOn: strm indent: level precedence: outerPrecedence [

	| parenthesize |
	parenthesize := precedence > outerPrecedence
		or: [outerPrecedence = 3 and: [precedence = 3 "both keywords"]].
	parenthesize
		ifTrue: [strm nextPutAll: '('.
				self printOn: strm indent: level.
				strm nextPutAll: ')']
		ifFalse: [self printOn: strm indent: level]
]

{ #category : #printing }
MessageNode >> printParenReceiver: rcvr on: aStream indent: level [
					
	rcvr isBlockNode ifTrue:
		[^rcvr printOn: aStream indent: level].
	aStream nextPut: $(.
	rcvr printOn: aStream indent: level.
	aStream nextPut: $)

]

{ #category : #printing }
MessageNode >> printReceiver: rcvr on: aStream indent: level [
					
	rcvr ifNil: [^ self].

	"Force parens around keyword receiver of kwd message"
	rcvr printOn: aStream indent: level precedence: precedence
]

{ #category : #printing }
MessageNode >> printRepeatOn: aStream indent: level [

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array new)
		on: aStream indent: level
]

{ #category : #printing }
MessageNode >> printToDoOn: aStream indent: level [

	| limitNode |
	self printReceiver: receiver on: aStream indent: level.

	(arguments last == nil or: [(arguments last isMemberOf: AssignmentNode) not])
		ifTrue: [limitNode := arguments first]
		ifFalse: [limitNode := arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printKeywords: #to:do:
					arguments: (Array with: limitNode with: (arguments at: 3))
					on: aStream indent: level]
		ifFalse: [self printKeywords: selector key
					arguments: (Array with: limitNode) , arguments allButFirst
					on: aStream indent: level]
]

{ #category : #printing }
MessageNode >> printWhileOn: aStream indent: level [

	self printReceiver: receiver on: aStream indent: level.
	(arguments isEmpty not
	 and: [arguments first isJust: NodeNil]) ifTrue:
			[selector := SelectorNode new
							key:
									(selector key == #whileTrue:
										ifTrue: [#whileTrue]
										ifFalse: [#whileFalse])
							code: #macro.
			arguments := Array new].
	self printKeywords: selector key arguments: arguments
		on: aStream indent: level
]

{ #category : #'equation translation' }
MessageNode >> receiver [
	^receiver
]

{ #category : #'equation translation' }
MessageNode >> receiver: val [
	"14 feb 2001 - removed return arrow"

	receiver := val
]

{ #category : #private }
MessageNode >> receiver: rcvr arguments: args precedence: p [

	receiver := rcvr.
	arguments := args.
	originalArguments := arguments copy.
	sizes := Array new: arguments size.
	precedence := p
]

{ #category : #'initialize-release' }
MessageNode >> receiver: rcvr selector: selNode arguments: args precedence: p [ 
	"Decompile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := selNode key.
	selNode code == #macro
		ifTrue: [self noteSpecialSelector: selNode key]
		ifFalse: [special := 0].
	selector := selNode.
	"self pvtCheckForPvtSelector: encoder"
	"We could test code being decompiled, but the compiler should've checked already. And where to send the complaint?"
]

{ #category : #'initialize-release' }
MessageNode >> receiver: rcvr selector: aSelector arguments: args precedence: p from: encoder [ 
	"Compile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := aSelector.
	self noteSpecialSelector: aSelector.
	(self transform: encoder)
		ifTrue: 
			[selector ifNil:
				[selector := SelectorNode new 
								key: (MacroSelectors at: special)
								code: #macro]]
		ifFalse: 
			[selector := encoder encodeSelector: aSelector.
			rcvr == NodeSuper ifTrue: [encoder noteSuper]].
	

]

{ #category : #'initialize-release' }
MessageNode >> receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range [
	"Compile."
	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder
]

{ #category : #'equation translation' }
MessageNode >> selector [
	^selector
]

{ #category : #'initialize-release' }
MessageNode >> selector: sel [
	selector := sel
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForCase: encoder value: forValue [

	| braceNode sizeIndex elseSize allReturn |
	forValue not ifTrue:
		[^super sizeCodeForEffect: encoder].
	equalNode := encoder encodeSelector: #=.
	braceNode := arguments first.
	sizes := Array new: 2 * braceNode numElements.
	sizeIndex := sizes size.
	elseSize := arguments size = 2
		ifTrue:
			[arguments last sizeCodeForEvaluatedValue: encoder] "otherwise: [...]"
		ifFalse:
			[caseErrorNode := encoder encodeSelector: #caseError.
			 (NodeSelf sizeCodeForValue: encoder)
			 + (caseErrorNode sizeCode: encoder args: 0 super: false)]. "self caseError"
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last |
		valueNode returns ifFalse: [allReturn := false]].
	braceNode casesReverseDo:
		[:keyNode :valueNode :last | | thenSize |
		sizes at: sizeIndex put: elseSize.
		thenSize := valueNode sizeCodeForEvaluatedValue: encoder.
		last ifFalse: [thenSize := thenSize + encoder sizePop].
		valueNode returns ifFalse: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		(last and: [allReturn]) ifTrue: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		sizes at: sizeIndex-1 put: thenSize.
		last ifFalse: [elseSize := elseSize + encoder sizeDup].
		elseSize := elseSize
					+ (keyNode sizeCodeForEvaluatedValue: encoder)
					+ (equalNode sizeCode: encoder args: 1 super: false)
					+ (self sizeCode: encoder forBranchOn: false dist: thenSize)
					+ thenSize.
		sizeIndex := sizeIndex - 2].
	^(receiver sizeCodeForValue: encoder) + elseSize
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForEffect: encoder [

	special > 0 
		ifTrue:
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: false].
	^super sizeCodeForEffect: encoder
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForIf: encoder value: forValue [
	| thenExpr elseExpr branchSize thenSize elseSize |
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	(forValue
	 or: [(thenExpr isJust: NodeNil)
	 or: [elseExpr isJust: NodeNil]]) not
			"(...not ifTrue: avoids using ifFalse: alone during this compile)"
		ifTrue:  "Two-armed IFs forEffect share a single pop"
			[^super sizeCodeForEffect: encoder].
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[elseSize := elseExpr sizeCodeForEvaluatedValue: encoder.
			thenSize := (thenExpr sizeCodeForEvaluatedValue: encoder)
					+ (thenExpr returns
						ifTrue: [0]  "Elide jump over else after a return"
						ifFalse: [self sizeCode: encoder forJump: elseSize]).
			branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[(elseExpr isJust: NodeNil)
				ifTrue:
					[elseSize := 0.
					thenSize := thenExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
				ifFalse:
					[thenSize := 0.
					elseSize := elseExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: true dist: elseSize]].
	sizes := Array with: thenSize with: elseSize.
	^(receiver sizeCodeForValue: encoder)
	+ branchSize + thenSize + elseSize
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForIfNil: encoder value: forValue [

	| theNode theSize theSelector |
	equalNode := encoder encodeSelector: #==.
	sizes := Array new: 1.
	theNode := arguments first.
	theSelector := #ifNotNil:.
	forValue
		ifTrue:
			[sizes at: 1 put: (theSize := (encoder sizePop + (theNode sizeCodeForEvaluatedValue: encoder))).
			 ^(receiver sizeCodeForValue: encoder)
			 + encoder sizeDup
			 + (encoder sizePushSpecialLiteral: nil)
			 + (equalNode sizeCode: encoder args: 1 super: false)
			 + (self 
					sizeCode: encoder forBranchOn: selector key == theSelector 
					dist: theSize)
			 + theSize]
		ifFalse:
			[sizes at: 1 put: (theSize := (theNode sizeCodeForEvaluatedEffect: encoder)).
			 ^(receiver sizeCodeForValue: encoder)
				+ (encoder sizePushSpecialLiteral: nil)
				+ (equalNode sizeCode: encoder args: 1 super: false)
				+ (self 
					sizeCode: encoder
					forBranchOn: selector key == theSelector 
					dist: theSize)
				+ theSize]
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForRepeat: encoder value: forValue [ 
	"L1: ... Jmp(L1) nil (nil for value only);"
	| loopSize |
	loopSize := (receiver sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: 1).
	sizes := Array with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForToDo: encoder value: forValue [ 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt test block incStmt blockSize initSize limitInit |
	block := arguments at: 3.
	initStmt := arguments at: 4.
	test := arguments at: 5.
	incStmt := arguments at: 6.
	limitInit := arguments at: 7.
	initSize := forValue
		ifTrue: [initStmt sizeCodeForValue: encoder.]
		ifFalse: [initStmt sizeCodeForEffect: encoder].
	limitInit == nil ifFalse:
		[initSize := initSize + (limitInit sizeCodeForEffect: encoder)].
	blockSize := (block sizeCodeForEvaluatedEffect: encoder)
			+ (incStmt sizeCodeForEffect: encoder)
			+ (encoder sizeJumpLong: -1).
	loopSize := (test sizeCodeForValue: encoder)
			+ (self sizeCode: encoder forBranchOn: false dist: blockSize)
			+ blockSize.
	sizes := Array with: blockSize with: loopSize.
	^initSize
	+ loopSize
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForValue: encoder [
	| total |
	special > 0 
		ifTrue: 
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: true].
	receiver == NodeSuper
		ifTrue: [selector := selector copy "only necess for splOops"].
	total := selector sizeCode: encoder args: arguments size super: receiver == NodeSuper.
	receiver == nil 
		ifFalse: [total := total + (receiver sizeCodeForValue: encoder)].
	sizes := arguments collect: 
					[:arg | | argSize | 
					argSize := arg sizeCodeForValue: encoder.
					total := total + argSize.
					argSize].
	^total
]

{ #category : #'code generation' }
MessageNode >> sizeCodeForWhile: encoder value: forValue [
        "L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only);
        justStmt, wholeLoop, justJump."
        | cond theArguments stmt stmtSize loopSize branchSize |
        cond := receiver.
        theArguments := arguments ifEmpty: [ Array with: (BlockNode withJust: NodeNil) ].
        stmt := theArguments at: 1.
        stmtSize := (stmt sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: 1).
        branchSize := self
                                        sizeCode: encoder
                                        forBranchOn: ((selector key == #whileFalse:) or: [ selector key == #whileFalse ])  "Btp for whileFalse"
                                        dist: stmtSize.
        loopSize := (cond sizeCodeForEvaluatedValue: encoder) + branchSize + stmtSize.
        sizes := Array with: stmtSize with: loopSize.
        ^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])
]

{ #category : #'macro transformations' }
MessageNode >> toDoFromWhileWithInit: initStmt [
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
	 and: [initStmt isAssignmentNode
	 and: [initStmt variable isTemp]]) ifFalse:
		[^nil].
	body := arguments last statements.
	variable := initStmt variable.
	increment := body last toDoIncrement: variable.
	(increment == nil
	 or: [receiver statements size ~= 1]) ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable = limit]]])
		ifTrue: [^nil]. 
	toDoBlock := BlockNode statements: body allButLast returns: false.
	toDoBlock arguments: (Array with: variable).
	variable scope: -1.
	variable beBlockArg.
	^MessageNode new
		receiver: initStmt value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: (Array with: limit with: increment with: toDoBlock)
		precedence: precedence
]

{ #category : #testing }
MessageNode >> toDoIncrement: variable [
	(receiver = variable and: [selector key = #+]) 
		ifFalse: [^ nil].
	arguments first isConstantNumber
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]
]

{ #category : #testing }
MessageNode >> toDoLimit: variable [
	(receiver = variable and: [selector key = #<= or: [selector key = #>=]]) 
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]
]

{ #category : #'macro transformations' }
MessageNode >> transform: encoder [
	special = 0 ifTrue: [^false].
	(self perform: (MacroTransformers at: special) with: encoder)
		ifTrue: 
			[^true]
		ifFalse: 
			[special := 0. ^false]
]

{ #category : #'macro transformations' }
MessageNode >> transformAnd: encoder [
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeFalse) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]
]

{ #category : #'macro transformations' }
MessageNode >> transformBoolean: encoder [
	^self
		checkBlock: (arguments at: 1)
		as: 'argument'
		from: encoder
]

{ #category : #'macro transformations' }
MessageNode >> transformCase: encoder [

	| caseNode |
	caseNode := arguments first.
	(caseNode isMemberOf: BraceNode) ifFalse: [ ^false ].
	(caseNode blockAssociationCheck: encoder) ifFalse: [ ^false ].
	(arguments size = 1 or: [
		self checkBlock: arguments last as: 'otherwise arg' from: encoder ]) ifFalse: [
			^false ].
	 caseNode elements do: [ :messageNode |
		messageNode receiver noteOptimizedIn: self.
		messageNode arguments first noteOptimizedIn: self ].
	 arguments size = 2 ifTrue: [ arguments last noteOptimizedIn: self ].
	 ^true
]

{ #category : #'macro transformations' }
MessageNode >> transformIfFalse: encoder [
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse:
			[^false]
]

{ #category : #'macro transformations' }
MessageNode >> transformIfFalseIfTrue: encoder [
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]
]

{ #category : #'macro transformations' }
MessageNode >> transformIfNil: encoder [

	"vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below."
	(MacroSelectors at: special) = #ifNotNil: ifTrue:
		[(self checkBlock: arguments first as: 'ifNotNil arg' from: encoder maxArgs: 1) ifFalse:
			[^false].

		"Transform 'ifNotNil: [stuff]' to 'ifNil: [nil] ifNotNil: [stuff]'.
		Slightly better code and more consistent with decompilation."
		self noteSpecialSelector: #ifNil:ifNotNil:.
		selector := SelectorNode new key: (MacroSelectors at: special) code: #macro.
		arguments := Array
						with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
						with: (arguments first noteOptimizedIn: self).
		(self transform: encoder) ifFalse:
			[self error: 'compiler logic error'].
		^true].
	(self checkBlock: arguments first as: 'ifNil arg' from: encoder) ifFalse:
		[^false].
	arguments first noteOptimizedIn: self.
	^true
]

{ #category : #'macro transformations' }
MessageNode >> transformIfNilIfNotNil: encoder [
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 2.
	((self checkBlock: (arguments at: 1) as: 'Nil arg' from: encoder)
	  and: [self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true
]

{ #category : #'macro transformations' }
MessageNode >> transformIfNotNilIfNil: encoder [
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true
]

{ #category : #'macro transformations' }
MessageNode >> transformIfTrue: encoder [
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]
]

{ #category : #'macro transformations' }
MessageNode >> transformIfTrueIfFalse: encoder [
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]]
]

{ #category : #'macro transformations' }
MessageNode >> transformOr: encoder [
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeTrue) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]
]

{ #category : #'macro transformations' }
MessageNode >> transformRepeat: encoder [
	"answer true if this #repeat message can be optimized"
	
	^(self checkBlock: receiver as: 'receiver' from: encoder)
	   and: [receiver noteOptimizedIn: self.
			true]
]

{ #category : #'macro transformations' }
MessageNode >> transformToDo: encoder [
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] 
Jmp(L1) L2: "
	| limit increment block initStmt test incStmt limitInit blockVar myRange blockRange limitIsAssignedTo |
	"First check for valid arguments"
	((arguments last isMemberOf: BlockNode)
	  and: [arguments last numberOfArguments = 1
	  and: [arguments last firstArgument isVariableReference "As with debugger remote vars"]]) ifFalse:
		[^false].
	arguments size = 3
		ifTrue: [increment := arguments at: 2.
				(increment isConstantNumber
				 and: [increment literalValue ~= 0]) ifFalse: [^false]]
		ifFalse: [increment := encoder encodeLiteral: 1].
	(limit := arguments at: 1) isVariableReference ifTrue:
		[limitIsAssignedTo := false.
		 arguments last nodesDo:
			[:node|
			(node isAssignmentNode and: [node variable = limit]) ifTrue:
				[limitIsAssignedTo := true]].
		 limitIsAssignedTo ifTrue:
			[^false]].
	arguments size < 3 ifTrue:   "transform to full form"
		[selector := SelectorNode new key: #to:by:do: code: #macro].

	"Now generate auxiliary structures"
	myRange := encoder rawSourceRanges at: self ifAbsent: [1 to: 0].
	block := arguments last.
	blockRange := encoder rawSourceRanges at: block ifAbsent: [1 to: 0].
	blockVar := block firstArgument.
	initStmt := AssignmentNode new variable: blockVar value: receiver.
	limit isVariableReference | limit isConstantNumber
		ifTrue: [limitInit := nil]
		ifFalse:  "Need to store limit in a var"
			[limit := encoder bindBlockArg: blockVar key, 'LimiT' within: block.
			 limit scope: -2.  "Already done parsing block; flag so it won't print"
			 block addArgument: limit.
			 limitInit := AssignmentNode new
							variable: limit
							value: arguments first].
	test := MessageNode new
				receiver: blockVar
				selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])
				arguments: (Array with: limit)
				precedence: precedence from: encoder
				sourceRange: (myRange first to: blockRange first).
	incStmt := AssignmentNode new
				variable: blockVar
				value: (MessageNode new
							receiver: blockVar selector: #+
							arguments: (Array with: increment)
							precedence: precedence from: encoder)
				from: encoder
				sourceRange: (myRange last to: myRange last).
	arguments := (Array with: limit with: increment with: block),
					(Array with: initStmt with: test with: incStmt with: limitInit).
	block noteOptimizedIn: self.
	^true
]

{ #category : #'macro transformations' }
MessageNode >> transformWhile: encoder [
	(self checkBlock: receiver as: 'receiver' from: encoder) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := SelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := Array with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]
]
