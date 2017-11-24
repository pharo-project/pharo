"
I decompile a method in three phases:
	Reverser: postfix byte codes -> prefix symbolic codes (nodes and atoms)
	Parser: prefix symbolic codes -> node tree (same as the compiler)
	Printer: node tree -> text (done by the nodes)
	

instance vars:

	constructor <DecompilerConstructor> an auxiliary knowing how to generate Abstract Syntax Tree (node tree)
	method <CompiledMethod> the method being decompiled
	instVars <Array of: String> the instance variables of the class implementing method
	tempVars <String | (OrderedCollection of: String)> hold the names of temporary variables (if known)
		NOTE: POLYMORPHISM WILL BE RESOLVED IN #initSymbols:
	constTable <Collection of: ParseNode> parse node associated with byte encoded constants (nil true false 0 1 -1 etc...)
	stack <OrderedCollection of: (ParseNode | String | Integer) > multipurpose...
	statements <OrderedCollection of: ParseNode> the statements of the method being decompiled 
	lastPc <Integer>
	exit <Integer>
	caseExits <OrderedCollection of: Integer> - stack of exit addresses that have been seen in the branches of caseOf:'s
	lastJumpPc <Integer>
	lastReturnPc <Integer>
	limit <Integer>
	hasValue <Boolean>
	blockStackBase <Integer>
	numLocaltemps <Integer | Symbol> - number of temps local to a block; also a flag indicating decompiling a block
	blockStartsToTempVars <Dictionary key: Integer value: (OrderedCollection of: String)>
	tempVarCount <Integer> number of temp vars used by the method
	lastJumpIfPcStack <OrderedCollection of: Integer> the value of program counter just before the last encountered conditional jumps
"
Class {
	#name : #Decompiler,
	#superclass : #InstructionStream,
	#instVars : [
		'constructor',
		'method',
		'instVars',
		'tempVars',
		'constTable',
		'stack',
		'statements',
		'lastPc',
		'exit',
		'caseExits',
		'lastJumpPc',
		'lastReturnPc',
		'limit',
		'hasValue',
		'blockStackBase',
		'numLocalTemps',
		'blockStartsToTempVars',
		'tempVarCount',
		'lastJumpIfPcStack'
	],
	#classVars : [
		'ArgumentFlag',
		'CascadeFlag',
		'CaseFlag',
		'IfNilFlag'
	],
	#category : #'Compiler-Kernel'
}

{ #category : #public }
Decompiler class >> decompile: aCompiledMethod [
	"Answers a decompiled AST for the compiled method"
	^ self new decompileMethod: aCompiledMethod
]

{ #category : #initialization }
Decompiler class >> initialize [

	CascadeFlag := 'cascade'.  "A unique object"
	CaseFlag := 'case'. "Ditto"
	ArgumentFlag := 'argument'.  "Ditto"
	IfNilFlag := 'ifNil'.  "Ditto"

	"Decompiler initialize"
]

{ #category : #control }
Decompiler >> blockForCaseTo: end [
	"Decompile a range of code as in statementsForCaseTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsForCaseTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block
]

{ #category : #'instruction decoding' }
Decompiler >> blockReturnConstant: value [ 
	self pushConstant: value;
		 blockReturnTop

]

{ #category : #'instruction decoding' }
Decompiler >> blockReturnTop [
	"No action needed"
]

{ #category : #private }
Decompiler >> blockScopeRefersOnlyOnceToTemp: offset [
	| nRefs byteCode extension scanner scan |
	scanner := InstructionStream on: method.
	nRefs := 0.
	scan := offset <= 15
				ifTrue:
					[byteCode := 16 + offset.
					 [:instr |
					  instr = byteCode ifTrue:
						[nRefs := nRefs + 1].
					  nRefs > 1]]
				ifFalse:
					[extension := 64 + offset.
					 [:instr |
					  (instr = 128 and: [scanner followingByte = extension]) ifTrue:
						[nRefs := nRefs + 1].
					   nRefs > 1]].
	self scanBlockScopeFor: pc from: method initialPC to: method endPC with: scan scanner: scanner.
	^nRefs = 1
]

{ #category : #control }
Decompiler >> blockTo: end [
	"Decompile a range of code as in statementsTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block
]

{ #category : #'instruction decoding' }
Decompiler >> case: dist [
	"statements = keyStmts CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns wierdly I guess)"
			stack removeLast. "get rid of CaseFlag"
			stmtStream := (self popTo: stack removeLast) readStream.
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].
]

{ #category : #control }
Decompiler >> checkForBlock: receiver selector: selector arguments: arguments [
	[selector == #closureCopy:copiedValues:] assert.
	^self checkForClosureCopy: receiver arguments: arguments
]

{ #category : #control }
Decompiler >> checkForClosureCopy: receiver arguments: arguments [
	"We just saw a closureCopy:copiedValues: message. Check for and construct a following block."

	| savePc jump |
	receiver == constructor codeThisContext ifFalse: [^false].
	savePc := pc.
	(jump := self interpretJump) notNil ifFalse:
		[pc := savePc.
		 ^nil].
	"Definitely a block"
	self doClosureCopyCopiedValues: arguments last "<BraceNode>" elements
		numArgs: arguments first key
		blockSize: jump.
	^true
]

{ #category : #private }
Decompiler >> constructorForMethod: aMethod [
	^DecompilerConstructorForClosures new
]

{ #category : #private }
Decompiler >> convertToDoLoop [
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| initStmt toDoStmt limitStmt |
	statements size < 2 ifTrue: [^ self].
	initStmt := statements at: statements size-1.
	(toDoStmt := statements last toDoFromWhileWithInit: initStmt)
		== nil ifTrue: [^ self].
	initStmt variable scope: -1.  "Flag arg as block temp"
	statements removeLast; removeLast; addLast: toDoStmt.

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...]"
	statements size < 2 ifTrue: [^ self].
	limitStmt := statements at: statements size-1.
	((limitStmt isMemberOf: AssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first
		and: [self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset]]])
		ifFalse: [^ self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements removeLast; removeLast; addLast: toDoStmt.


]

{ #category : #private }
Decompiler >> decompile: aSelector in: aClass method: aMethod [

	^self
		decompile: aSelector
		in: aClass
		method: aMethod
		using: (self constructorForMethod: aMethod)
]

{ #category : #private }
Decompiler >> decompile: aSelector in: aClass method: aMethod using: aConstructor [

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new: method frameSize.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new: 20.
			numLocalTemps := 0.
			super method: method pc: method initialPC.
			"skip primitive error code store if necessary"
			(method isPrimitive and: [self willStore]) ifTrue:
				[pc := pc + 2.
				 tempVars := tempVars asOrderedCollection].
			block := self blockTo: method endPC + 1.
			stack isEmpty ifFalse: [self error: 'stack not empty']].
	node := constructor
				codeMethod: aSelector
				block: block
				tempVars: tempVars
				primitive: method primitive
				class: aClass.
	method isPrimitive ifTrue:
		[node removeAndRenameLastTempIfErrorCode].
	^node preen
]

{ #category : #'public access' }
Decompiler >> decompileMethod: aMethod [

	^self
		decompile: aMethod selector
		in: aMethod methodClass
		method: aMethod
]

{ #category : #control }
Decompiler >> doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize [
	| startpc savedTemps savedTempVarCount savedNumLocalTemps
	  jump blockArgs blockTemps blockTempsOffset block |
	savedTemps := tempVars.
	savedTempVarCount := tempVarCount.
	savedNumLocalTemps := numLocalTemps.
	jump := blockSize + (startpc := pc).
	numLocalTemps := method encoderClass numLocalTempsForBlockAt: startpc in: method.
	blockTempsOffset := numArgs + blockCopiedValues size.
	(blockStartsToTempVars notNil "implies we were intialized with temp names."
	 and: [blockStartsToTempVars includesKey: pc])
		ifTrue:
			[tempVars := blockStartsToTempVars at: pc]
		ifFalse:
			[blockArgs := (1 to: numArgs) collect:
							[:i| (constructor
									codeTemp: i - 1
									named: 't', (tempVarCount + i) printString)
								  beBlockArg].
			blockTemps := (1 to: numLocalTemps) collect:
							[:i| constructor
									codeTemp: i + blockTempsOffset - 1
									named: 't', (tempVarCount + i + numArgs) printString].
			tempVars := blockArgs, blockCopiedValues, blockTemps].
	numLocalTemps timesRepeat:
		[self interpretNextInstructionFor: self.
		 stack removeLast].
	tempVarCount := tempVarCount + numArgs + numLocalTemps.
	block := self blockTo: jump.
	stack addLast: ((constructor
						codeArguments: (tempVars copyFrom: 1 to: numArgs)
						temps: (tempVars copyFrom: blockTempsOffset + 1 to: blockTempsOffset + numLocalTemps)
						block: block)
							pc: startpc;
							yourself).
	tempVars := savedTemps.
	tempVarCount := savedTempVarCount.
	numLocalTemps := savedNumLocalTemps
]

{ #category : #'instruction decoding' }
Decompiler >> doDup [

	stack last == CascadeFlag
		ifFalse:
			["Save position and mark cascade"
			stack addLast: statements size.
			stack addLast: CascadeFlag].
	stack addLast: CascadeFlag
]

{ #category : #'instruction decoding' }
Decompiler >> doNop [

	"do nothing"
]

{ #category : #'instruction decoding' }
Decompiler >> doPop [

	stack isEmpty ifTrue:
		["Ignore pop in first leg of ifNil for value"
		^ self].
	stack last == CaseFlag
		ifTrue: [stack removeLast]
		ifFalse: [statements addLast: stack removeLast].
]

{ #category : #'instruction decoding' }
Decompiler >> doStore: stackOrBlock [
	"Only called internally, not from InstructionStream. StackOrBlock is stack
	for store, statements for storePop."

	| var expr |
	var := stack removeLast.
	expr := stack removeLast.
	stackOrBlock addLast: (expr == ArgumentFlag
		ifTrue: [var]
		ifFalse: [constructor codeAssignTo: var value: expr])
]

{ #category : #'initialize-release' }
Decompiler >> initSymbols: aClass [
	constructor method: method class: aClass literals: method literals.
	constTable := constructor codeConstants.
	instVars := Array new: aClass instSize.
	tempVarCount := method numTemps.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars := self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars := blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps := tempVars ifNil: [(1 to: tempVarCount) collect: [:i| 't', i printString]].
			tempVars := (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i) beMethodArg]
]

{ #category : #'instruction decoding' }
Decompiler >> jump: dist [
	| blockBody destPc nextPC |
	destPc := pc + dist.
	(lastJumpIfPcStack isEmpty or: [dist < 0 and: [destPc > lastJumpIfPcStack last]])
		ifTrue:
			["Rule: aBackward jump not crossing a Bfp/Btp must be a repeat"
			nextPC := pc.
			pc := destPc.
			blockBody := self statementsTo: lastPc.
			blockBody size timesRepeat: [statements removeLast].
			pc := nextPC.
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: #repeat
								code: #macro)
					arguments: #()).
			]
		ifFalse:
			[exit := destPc.
			lastJumpPc := lastPc]
]

{ #category : #'instruction decoding' }
Decompiler >> jump: dist if: condition [

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack blockBody |
	lastJumpIfPcStack addLast: lastPc.
	stack last == CascadeFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
	elsePc := lastPc.
	elseStart := pc + dist.
	end := limit.
	"Check for bfp-jmp to invert condition.
	Don't be fooled by a loop with a null body."
	sign := condition.
	savePc := pc.
	self interpretJump ifNotNil:
		[:elseDist|
		 (elseDist >= 0 and: [elseStart = pc]) ifTrue:
			 [sign := sign not.  elseStart := pc + elseDist]].
	pc := savePc.
	ifExpr := stack removeLast.
	(isIfNil := stack size > 0 and: [stack last == IfNilFlag]) ifTrue:
		[stack removeLast].
	saveStack := stack.
	stack := OrderedCollection new.
	thenBlock := self blockTo: elseStart.
	condHasValue := hasValue or: [isIfNil].
	"ensure jump is within block (in case thenExpr returns)"
	thenJump := exit <= end ifTrue: [exit] ifFalse: [elseStart].
	"if jump goes back, then it's a loop"
	thenJump < elseStart
		ifTrue:
			["Must be a while loop...
			  thenJump will jump to the beginning of the while expr.  In the case of while's
			  with a block in the condition, the while expr should include more than just
			  the last expression: find all the statements needed by re-decompiling."
			stack := saveStack.
			pc := thenJump.
			blockBody := self statementsTo: elsePc.
			"discard unwanted statements from block"
			blockBody size - 1 timesRepeat: [statements removeLast].
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: (sign
												ifTrue: [#whileFalse:]
												ifFalse: [#whileTrue:])
								code: #macro)
					arguments: { thenBlock }).
			pc := elseStart.
			self convertToDoLoop]
		ifFalse:
			["Must be a conditional..."
			elseBlock := self blockTo: thenJump.
			elseJump := exit.
			"if elseJump is backwards, it is not part of the elseExpr"
			elseJump < elsePc ifTrue:
				[pc := lastPc].
			cond := isIfNil
						ifTrue:
							[constructor
								codeMessage: ifExpr ifNilReceiver
								selector: (constructor
											codeSelector: (sign ifTrue: [#ifNotNil:] ifFalse: [#ifNil:])
											code: #macro)
								arguments: (Array with: thenBlock)]
						ifFalse:
							[constructor
								codeMessage: ifExpr
								selector: (constructor codeSelector: #ifTrue:ifFalse: code: #macro)
								arguments:	(sign
												ifTrue: [{elseBlock. thenBlock}]
												ifFalse: [{thenBlock. elseBlock}])].
			stack := saveStack.
			condHasValue
				ifTrue: [stack addLast: cond]
				ifFalse: [statements addLast: cond]].
	lastJumpIfPcStack removeLast.
]

{ #category : #'initialize-release' }
Decompiler >> mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor [
	| map |
	map := aMethod debuggerMap
				mapFromBlockKeys: (aMethod debuggerMap startpcsToBlockExtents: aMethod) keys asArray sort
				toSchematicTemps: schematicTempNamesString
				method: aMethod.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector|
		tempNameTupleVector isEmpty ifFalse:
			[| subMap numTemps tempVector |
			subMap := Dictionary new.
			"Find how many temp slots there are (direct & indirect temp vectors)
			 and for each indirect temp vector find how big it is."
			tempNameTupleVector do:
				[:tuple|
				tuple last isArray
					ifTrue:
						[subMap at: tuple last first put: tuple last last.
						 numTemps := tuple last first]
					ifFalse:
						[numTemps := tuple last]].
			"create the temp vector for this scope level."
			tempVector := Array new: numTemps.
			"fill it in with any indirect temp vectors"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector at: index put: (Array new: size)].
			"fill it in with temp nodes."
			tempNameTupleVector do:
				[:tuple| | itv |
				tuple last isArray
					ifTrue:
						[itv := tempVector at: tuple last first.
						 itv at: tuple last last
							put: (aDecompilerConstructor
									codeTemp: tuple last last - 1
									named: tuple first)]
					ifFalse:
						[tempVector
							at: tuple last
							put: (aDecompilerConstructor
									codeTemp: tuple last - 1
									named: tuple first)]].
			"replace any indirect temp vectors with proper RemoteTempVectorNodes"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector
					at: index
					put: (aDecompilerConstructor
							codeRemoteTemp: index
							remoteTemps: (tempVector at: index))].
			"and update the entry in the map"
			map at: startpc put: tempVector]].
	^map
]

{ #category : #'instruction decoding' }
Decompiler >> methodReturnConstant: value [

	self pushConstant: value; methodReturnTop
]

{ #category : #'instruction decoding' }
Decompiler >> methodReturnReceiver [

	self pushReceiver; methodReturnTop
]

{ #category : #'instruction decoding' }
Decompiler >> methodReturnTop [
	| last |
	last := stack removeLast "test test" asReturnNode.
	stack size > blockStackBase  "get effect of elided pop before return"
		ifTrue: [statements addLast: stack removeLast].
	exit := pc.
	lastJumpPc := lastReturnPc := lastPc.
	statements addLast: last
]

{ #category : #'instruction decoding' }
Decompiler >> popIntoLiteralVariable: value [

	self pushLiteralVariable: value; doStore: statements
]

{ #category : #'instruction decoding' }
Decompiler >> popIntoReceiverVariable: offset [

	self pushReceiverVariable: offset; doStore: statements
]

{ #category : #'instruction decoding' }
Decompiler >> popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	self sawClosureBytecode.
	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: statements
]

{ #category : #'instruction decoding' }
Decompiler >> popIntoTemporaryVariable: offset [
	| maybeTVTag tempVector start |
	maybeTVTag := stack last.
	((maybeTVTag isMemberOf: Association)
	 and: [maybeTVTag key == #pushNewArray]) ifTrue:
		[blockStartsToTempVars notNil "implies we were intialized with temp names."
			ifTrue: "Use the provided temps"
				[[(tempVector := tempVars at: offset + 1 ifAbsent: [ParseNode basicNew]) isTemp
							 and: [tempVector isIndirectTempVector
							 and: [tempVector remoteTemps size = maybeTVTag value size]]] assert]
			ifFalse: "Synthesize some remote temps"
				[tempVector := maybeTVTag value.
				 offset + 1 <= tempVars size
					ifTrue:
						[start := 2.
						 tempVector at: 1 put: (tempVars at: offset + 1)]
					ifFalse:
						[tempVars := (Array new: offset + 1)
										replaceFrom: 1
										to: tempVars size
										with: tempVars.
						start := 1].
				 start to: tempVector size do:
					[:i|
					tempVector
						at: i
						put: (constructor
								codeTemp: numLocalTemps + offset + i - 1
								named: 't', (tempVarCount + i) printString)].
				tempVars at: offset + 1 put: (constructor codeRemoteTemp: offset + 1 remoteTemps: tempVector)].
		 tempVarCount := tempVarCount + maybeTVTag value size.
		 stack removeLast.
		 ^self].
	self pushTemporaryVariable: offset; doStore: statements
]

{ #category : #private }
Decompiler >> popTo: oldPos [

	| t |
	t := Array new: statements size - oldPos.
	(t size to: 1 by: -1) do:
		[:i | t at: i put: statements removeLast].
	^t
]

{ #category : #'instruction decoding' }
Decompiler >> pushActiveContext [

	stack addLast: constructor codeThisContext
]

{ #category : #'instruction decoding' }
Decompiler >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [
	| copiedValues |
	self sawClosureBytecode.
	copiedValues := ((1 to: numCopied) collect: [:ign| stack removeLast]) reversed.
	self doClosureCopyCopiedValues: copiedValues numArgs: numArgs blockSize: blockSize
]

{ #category : #'instruction decoding' }
Decompiler >> pushConsArrayWithElements: numElements [ 
	| array |
	self sawClosureBytecode.
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: stack removeLast].
	stack addLast: (constructor codeBrace: array)
]

{ #category : #'instruction decoding' }
Decompiler >> pushConstant: value [

	| node |
	node := value == true ifTrue: [constTable at: 2]
		ifFalse: [value == false ifTrue: [constTable at: 3]
		ifFalse: [value == nil ifTrue: [constTable at: 4]
		ifFalse: [constructor codeAnyLiteral: value]]].
	stack addLast: node
]

{ #category : #'instruction decoding' }
Decompiler >> pushLiteralVariable: assoc [

	stack addLast: (constructor codeAnyLitInd: assoc)
]

{ #category : #'instruction decoding' }
Decompiler >> pushNewArrayOfSize: size [
	self sawClosureBytecode.
	stack addLast: #pushNewArray -> (Array new: size)
]

{ #category : #'instruction decoding' }
Decompiler >> pushReceiver [

	stack addLast: (constTable at: 1)
]

{ #category : #'instruction decoding' }
Decompiler >> pushReceiverVariable: offset [

	| var |
	(var := instVars at: offset + 1 ifAbsent: []) == nil
		ifTrue:
			["Not set up yet"
			var := constructor codeInst: offset.
			instVars size < (offset + 1) ifTrue: [
				instVars := (Array new: offset + 1)
					replaceFrom: 1 to: instVars size with: instVars; yourself ].
			instVars at: offset + 1 put: var].
	stack addLast: var
]

{ #category : #'instruction decoding' }
Decompiler >> pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	self sawClosureBytecode.
	stack addLast: ((tempVars at: tempVectorIndex + 1) remoteTemps at: remoteTempIndex + 1)
]

{ #category : #'instruction decoding' }
Decompiler >> pushTemporaryVariable: offset [

	stack addLast: (tempVars at: offset + 1)
]

{ #category : #private }
Decompiler >> quickMethod [
	| |
	method isReturnSpecial
		ifTrue: [^ constructor codeBlock:
				(Array with: (constTable at: method primitive - 255)) returns: true].
	method isReturnField
		ifTrue: [^ constructor codeBlock:
				(Array with: (constructor codeInst: method returnField)) returns: true].
	self error: 'improper short method'
]

{ #category : #private }
Decompiler >> sawClosureBytecode [
	constructor isForClosures ifFalse:
		[constructor primitiveChangeClassTo: DecompilerConstructorForClosures new]
]

{ #category : #private }
Decompiler >> scanBlockScopeFor: refpc from: startpc to: endpc with: scan scanner: scanner [
	| bsl maybeBlockSize |
	bsl := BlockStartLocator new.
	scanner pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[refpc = scanner pc ifTrue:
			[scanner pc: startpc.
			 [scanner pc <= endpc] whileTrue:
				[(scan value: scanner firstByte) ifTrue:
					[^endpc].
				 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
					[scanner pc: scanner pc + maybeBlockSize]].
			   ^self].
		 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
			[refpc <= (scanner pc + maybeBlockSize)
				ifTrue: [^self scanBlockScopeFor: refpc from: scanner pc to: scanner pc + maybeBlockSize with: scan scanner: scanner]
				ifFalse: [scanner pc: scanner pc + maybeBlockSize]]]
]

{ #category : #'instruction decoding' }
Decompiler >> send: selector super: superFlag numArgs: numArgs [

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	((#(closureCopy:copiedValues:) includes: selector)
	  and: [self checkForBlock: rcvr selector: selector arguments: args]) ifFalse:
		[selNode := constructor codeAnySelector: selector.
		rcvr == CascadeFlag
			ifTrue:
				["May actually be a cascade or an ifNil: for value."
				self willJumpIfFalse
					ifTrue: "= generated by a case macro"
						[selector == #= ifTrue:
							[" = signals a case statement..."
							statements addLast: args first.
							stack addLast: rcvr. "restore CascadeFlag"
							^ self].
						selector == #== ifTrue:
							[" == signals an ifNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]]
					ifFalse:
						[(self willJumpIfTrue and: [selector == #==]) ifTrue:
							[" == signals an ifNotNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]].
				msgNode := constructor
								codeCascadedMessage: selNode
								arguments: args.
				stack last == CascadeFlag ifFalse:
					["Last message of a cascade"
					statements addLast: msgNode.
					messages := self popTo: stack removeLast.  "Depth saved by first dup"
					msgNode := constructor
									codeCascade: stack removeLast
									messages: messages]]
			ifFalse:
				[msgNode := constructor
							codeMessage: rcvr
							selector: selNode
							arguments: args].
		stack addLast: msgNode]
]

{ #category : #control }
Decompiler >> statementsForCaseTo: end [
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end.
	Note that stack initially contains a CaseFlag which will be removed by
	a subsequent Pop instruction, so adjust the StackPos accordingly."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size - 1. "Adjust for CaseFlag"
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[stack last == CaseFlag
				ifFalse: [ statements addLast: stack removeLast] ].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	caseExits add: exit.
	^self popTo: blockPos
]

{ #category : #control }
Decompiler >> statementsTo: end [
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size.
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[statements addLast: stack removeLast].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	^self popTo: blockPos
]

{ #category : #'instruction decoding' }
Decompiler >> storeIntoLiteralVariable: assoc [

	self pushLiteralVariable: assoc; doStore: stack
]

{ #category : #'instruction decoding' }
Decompiler >> storeIntoReceiverVariable: offset [

	self pushReceiverVariable: offset; doStore: stack
]

{ #category : #'instruction decoding' }
Decompiler >> storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	self sawClosureBytecode.
	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: stack
]

{ #category : #'instruction decoding' }
Decompiler >> storeIntoTemporaryVariable: offset [

	self pushTemporaryVariable: offset; doStore: stack
]

{ #category : #'public access' }
Decompiler >> tempAt: offset [
	"Needed by BraceConstructor<PopIntoTemporaryVariable"

	^tempVars at: offset + 1
]
