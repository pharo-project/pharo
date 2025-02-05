"
Handles the byte code generation of jumps, literals and similar things. Byte code independent. The encoder is bytecode dependent.

I generate bytecodes in response to 'instructions' messages being sent to me.  I rewrite jumps at the end so their jump offsets are correct (see #bytecodes).  For example, to create a compiled method that compares first instVar to first arg and returns 'yes' or 'no' (same example as in IRBuilder), do:

```
	BytecodeGenerator new
		numArgs: 1;
		pushInstVar: 1;
		pushTemp: 1;
		send: #>;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod
```

You can send #ir to the compiledMethod to decompile to its IRMethod, and you can send #methodNode to either to decompile to its parse tree.


### Instance Variables
	additionalLiterals:		<OCLiteralSet> Those are literals evaluated for effect and optimized control flow messages selectors used in the method in order to be able to do sendersOf: on these literals with success.
	bytes:		<OrderedCollection of bytes> current sequence of bytecodes being written 
	encoder:		<BytecodeEncoder> Set the one you need depending on which bytecode set you want to use.
	forceLongForm:		<Boolean> true if instance variable access requires a long form (typically, Context access)
		
	Following inst vars are used to map bytecode pc to IR instruction	
	instrMap:		<OrderedCollection>
	instrMaps:		<IdentityDictionary of OrderedCollection>
			
	lastLiteral:		<Behavior> Basically in the case you would compile without annotating the method with its class and its selector, this literal is used to enforce the method class to be a literal to allow super sends.
	lastSpecialReturn:		<Message> used to generate quick returns
	literals:		<OCLiteralList> literals of the methods
	numArgs:		<Smi> number of arguments of the method
	numberOfTemps:		<Smi> number of temps of the method
	primNumber:		<Smi> primitive number (or 0)
	properties:		<AdditionalMethodState | nil> used to hold the additional method state (pragmas for examples)
			
	Following inst vars are used to correctly map the jumps (See #initialize for extra information)
	orderSeq:		<OrderedCollection> Reverse map of seq order
	seqBytes:		<IdentityDictionary>
	seqOrder:		<IdentityDictionary>
	jumps:		<IdentityDictionary>
	currentSeqId:		<Object>
	currentSeqNum:		<Smi>
			
	stack:		<Stack> simulated stack. Only simulates the depth of the stack. Store its maximum value in stacks instance variable.
	stacks:		<IdentityDictionary (seqId -> stackCount)> used to find out the maximum depth of the method and therefore set the largeFrameBit
"
Class {
	#name : 'OCIRBytecodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'encoder',
		'seqOrder',
		'orderSeq',
		'seqBytes',
		'jumps',
		'literals',
		'lastLiteral',
		'currentSeqId',
		'currentSeqNum',
		'bytes',
		'lastSpecialReturn',
		'primitiveBytes',
		'instrMaps',
		'instrMap',
		'stacks',
		'stack',
		'numArgs',
		'properties',
		'numberOfTemps',
		'forceLongForm',
		'primNumber',
		'encoderClass',
		'inBlock',
		'compilationContext'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'instance creation' }
OCIRBytecodeGenerator class >> newWithEncoderClass: class [
	^ self basicNew
		encoderClass: class;
		initialize;
		yourself
]

{ #category : 'private' }
OCIRBytecodeGenerator >> addLastLiteral: object [
	lastLiteral ifNil: [ ^ lastLiteral := object ].
	((lastLiteral literalEqual: object)
		or: [
			"case of metaclass, they have no unique association"
			lastLiteral isVariableBinding and: [ lastLiteral key isNil ] ])
		ifFalse: [ self error: 'there can only be one last literal' ]
]

{ #category : 'private' }
OCIRBytecodeGenerator >> addLiteral: object [
	^ literals addLiteral: object
]

{ #category : 'private' }
OCIRBytecodeGenerator >> addPragma: aPragma [
	properties ifNil: [ properties := AdditionalMethodState new ].
	properties := properties copyWith: aPragma
]

{ #category : 'results' }
OCIRBytecodeGenerator >> addProperties: cm [
	"set the properties of cm according to properties saved"

	properties
		ifNotNil: [ cm penultimateLiteral: properties.
			properties method: cm.
			properties pragmas do: [ :each | each method: cm ] ]
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> blockReturnTop [
	encoder genReturnTopToCaller
]

{ #category : 'results' }
OCIRBytecodeGenerator >> bytecodes [
	| stream |
	self updateJumpOffsets.
	stream := (ByteArray new: 100) writeStream.
	primitiveBytes ifNotNil: [stream nextPutAll: primitiveBytes contents].
	orderSeq do: [ :seqId | |lastInstr|
			"If the instruction will emit bytes, record the first byte emitted as the instructions bytecode index"
			lastInstr := OCIRInstruction new -> 1.
			(instrMaps at: seqId) do: [ :assoc | |nextInstr|
				nextInstr := assoc.
				lastInstr key bytecodeIndex: ((nextInstr value - lastInstr value) > 0 ifTrue: [
					stream position + lastInstr value]).
				lastInstr := assoc.
				  ].
			lastInstr key bytecodeIndex: ((seqBytes at: seqId) size < lastInstr value ifFalse: [  stream position + lastInstr value]).
			stream nextPutAll: (seqBytes at: seqId) ].
	^ stream contents
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> compilationContext [
	^ compilationContext ifNil: [compilationContext := OCCompilationContext default]
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : 'results' }
OCIRBytecodeGenerator >> compiledBlock [
	| lits quickPrimitive header cb |
	lits := self literals allButLast "1 free only for compiledBlock".
	quickPrimitive := self quickMethodPrim.
	"disabled for now"
	"(primNumber = 0 and: [quickPrimitive > 0]) ifTrue: [
		""if we are a quick method we need to add a bytecode at the beginning. #bytecodes
		takes primitiveBytes into account""
		primitiveBytes := (ByteArray new: 4) writeStream.
		encoder stream: primitiveBytes.
		encoder genCallPrimitive: quickPrimitive.
	]."
	header := self spurVMHeader: lits size.
	cb := CompiledBlock basicNew: self bytecodes size header: header.
	(WriteStream with: cb)
		position: cb initialPC - 1;
		nextPutAll: self bytecodes.
	lits withIndexDo: [:obj :i | cb literalAt: i put: obj].
	cb needsFrameSize: self stackFrameSize.
	self updateOuterCodeBlocks: cb.
	^ cb
]

{ #category : 'results' }
OCIRBytecodeGenerator >> compiledMethod [
	| lits quickPrimitive |
	lits := self literals.
	quickPrimitive := self quickMethodPrim.
	(primNumber = 0 and: [quickPrimitive > 0]) ifTrue: [
		"if we are a quick method we need to add a bytecode at the beginning. #bytecodes
		takes primitiveBytes into account"
		primitiveBytes := (ByteArray new: 4) writeStream.
		encoder stream: primitiveBytes.
		encoder genCallPrimitive: quickPrimitive.
	].
	^ self compiledMethodHeader: (self spurVMHeader: lits size) literals: lits
]

{ #category : 'results' }
OCIRBytecodeGenerator >> compiledMethodHeader: header literals: lits [
	| cm cmClass|
	"we look up the class in the productionEnvironment"
	cmClass := self compilationContext compiledMethodClass.
	cm := cmClass basicNew: self bytecodes size + CompiledMethod trailerSize header: header.
	(WriteStream with: cm)
		position: cm initialPC - 1;
		nextPutAll: self bytecodes.
	lits withIndexDo: [:obj :i | cm literalAt: i put: obj].
	cm needsFrameSize: self stackFrameSize.
	self updateOuterCodeBlocks: cm.
	self addProperties: cm.
	^ cm
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> encoderClass [
	^ encoderClass
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> encoderClass: class [
	encoderClass := class
]

{ #category : 'results' }
OCIRBytecodeGenerator >> endPrimNumber [
	"primitive number including quick return. Only valid at the end of generation"
	^ primNumber > 0
			ifTrue: [ primNumber ]
			ifFalse: [ self quickMethodPrim ]
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> forceLongForm: aBoolean [
	forceLongForm := aBoolean
]

{ #category : 'private' }
OCIRBytecodeGenerator >> from: fromSeqId goto: toSeqId [

	| distance from to |
	from := seqOrder at: fromSeqId.
	to := seqOrder at: toSeqId ifAbsent: [^ self].
	from + 1 = to ifTrue: [^ self].  "fall through, no jump needed"

	from < to ifTrue: [ "jump forward"
		distance := (from + 1 to: to - 1) inject: 0 into: [:size :i |
				size + (seqBytes at: (orderSeq at: i)) size].
		self jumpForward: distance.
	] ifFalse: [ "jump backward"
		distance := ((to to: from - 1) inject: 0 into: [:size :i |
				size + (seqBytes at: (orderSeq at: i)) size])
			+ bytes size.
		self jumpBackward: distance.
	]
]

{ #category : 'private' }
OCIRBytecodeGenerator >> from: fromSeqId if: bool goto: toSeqId otherwise: otherwiseSeqId [

	| distance from to otherwise |
	from := seqOrder at: fromSeqId.
	to := seqOrder at: toSeqId ifAbsent: [^ self jump: 0 if: bool].  "not done yet"
	otherwise := seqOrder at: otherwiseSeqId ifAbsent: [^ self jump: 0 if: bool].  "not done yet"
	from < to ifFalse: [self error].
	from + 1 = otherwise ifFalse: [self error].
	distance := (from + 1 to: to - 1)
		inject: 0
		into: [:size :i | size + (seqBytes at: (orderSeq at: i)) size].
	self jump: distance if: bool
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> goto: seqId [
	stacks at: seqId put: (stack linkTo: (stacks at: seqId ifAbsentPut: [ nil ])).
	self
		saveLastJump:
			(Message
				selector: #from:goto:
				arguments:
					{currentSeqId.
					seqId}).
	self from: currentSeqId goto: seqId
]

{ #category : 'results' }
OCIRBytecodeGenerator >> hasPrimitive [
	"do I have a primitive? Both normal primitive and quick return"
	primNumber > 0 ifTrue: [ ^true ].
	^ self endPrimNumber > 0
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> if: bool goto: seqId [
	| otherwiseSeqId |
	otherwiseSeqId := self newDummySeqId.
	self if: bool goto: seqId otherwise: otherwiseSeqId.
	self label: otherwiseSeqId
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> if: bool goto: seqId1 otherwise: seqId2 [
	stack pop.
	stacks at: seqId1 put: (stack linkTo: (stacks at: seqId1 ifAbsentPut: [ nil ])).
	stacks at: seqId2 put: (stack linkTo: (stacks at: seqId2 ifAbsentPut: [ nil ])).
	self
		saveLastJump:
			(Message
				selector: #from:if:goto:otherwise:
				arguments:
					{currentSeqId.
					bool.
					seqId1.
					seqId2}).
	self
		from: currentSeqId
		if: bool
		goto: seqId1
		otherwise: seqId2
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> inBlock: bool [
	inBlock := bool
]

{ #category : 'initialization' }
OCIRBytecodeGenerator >> initialize [

	super initialize.

	encoder := self encoderClass new.
	encoder stream: self.

	primNumber := 0.

	inBlock := false.
	literals := OCLiteralList new.
	"The following dicts are keyed by sequence id given by client in label: (and gotos)."
	seqOrder := IdentityDictionary new.  "seqId -> seq order num"
	seqBytes := IdentityDictionary new.  "seqId -> seq bytecodes"
	jumps := IdentityDictionary new.  "seqId -> last jump instr"
	instrMaps := IdentityDictionary new.  "seqId -> (clientInstr -> bytecode pos)"
	stacks := IdentityDictionary new.  "seqId -> stackCount"
	numArgs := 0.
	currentSeqNum := 0.
	orderSeq := OrderedCollection new.  "reverse map of seqOrder"
	forceLongForm := false.
	"starting label in case one is not provided by client"
	self label: self newDummySeqId
]

{ #category : 'initialization' }
OCIRBytecodeGenerator >> irPrimitive: anIrPrimitive [
	literals isEmpty ifFalse: [self error: 'init prim before adding instructions'].
	anIrPrimitive spec ifNotNil: [literals addLiteral: anIrPrimitive spec].
	primNumber := anIrPrimitive num.
	primNumber = 0 ifTrue: [ ^ self ].
	self encoderClass callPrimitiveCode ifNil: [ ^ self ].
	encoder genCallPrimitive: primNumber
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> jump: distance if: condition [
	distance = 0
		ifTrue: [ ^ encoder genPop ].
	condition
		ifTrue: [ encoder genBranchPopTrue: distance ]
		ifFalse: [ encoder genBranchPopFalse: distance ]
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> jump: distance if: condition withInterpreter: anInterpreter [

	^ self jump: distance if: condition
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> jumpBackward: distance [

	encoder genJumpLong: distance negated - self encoderClass backJumpBytecodeSize
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> jumpForward: distance [

	distance = 0 ifTrue: [^ self].
	encoder genJump: distance
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> label: seqId [
	lastSpecialReturn := nil.
	currentSeqId := seqId.
	currentSeqNum := currentSeqNum + 1.
	seqOrder at: seqId put: currentSeqNum.
	orderSeq at: currentSeqNum ifAbsentPut: [seqId].
	bytes := seqBytes at: seqId ifAbsentPut: [OrderedCollection new].
	jumps at: seqId ifAbsentPut: [nil].
	instrMap := instrMaps at: seqId ifAbsentPut: [OrderedCollection new].
	stack
		ifNil: [ stack := stacks at: currentSeqId ifAbsentPut: [ OCIRStackCount new ] ]
		ifNotNil: [stack := stacks at: currentSeqId ifAbsentPut: [ stack class newOn:stack ] ]
]

{ #category : 'private' }
OCIRBytecodeGenerator >> literalIndexOf: object [

	| index |

	index := literals literalIndexOf: object ifAbsent: [self addLiteral: object].

	"conversion from 1 based to 0 based"
	^ index - 1
]

{ #category : 'results' }
OCIRBytecodeGenerator >> literals [
	literals := literals asArray.
	(literals anySatisfy: [ :each | each isMethodProperties ])
		ifFalse: [ literals := literals copyWith: nil ].
	^ lastLiteral ifNil: [ literals copyWith: nil ] ifNotNil: [ literals copyWith: lastLiteral ]
]

{ #category : 'mapping' }
OCIRBytecodeGenerator >> mapBytesTo: instr [
	"Record the current byte offset in instruction sequence as start of instr.
	This is later used to calculate the total byte offset of instruction in generated code,
	see #bytecodes"
	instrMap add: instr -> (bytes size + 1)
]

{ #category : 'private' }
OCIRBytecodeGenerator >> newDummySeqId [

	^ Object new
]

{ #category : 'private' }
OCIRBytecodeGenerator >> nextPut: byte [
	bytes add: byte
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> numArgs [

	^ numArgs
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> numArgs: n [

	numArgs := n
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> numTemps [

	^ numberOfTemps
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> numTemps: anInteger [

	numberOfTemps := anInteger
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> popTop [
	stack pop.
	encoder genPop
]

{ #category : 'private' }
OCIRBytecodeGenerator >> pragmas: aCollection [
	aCollection do: [:each | self addPragma: each]
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> primNum [
	^ primNumber
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> properties [
	^ properties
]

{ #category : 'accessing' }
OCIRBytecodeGenerator >> properties: propDict [
	properties := propDict
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushConsArray: size [
	stack push.
	stack pop: size.
	encoder genPushConsArray: size
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushDup [
	stack push.
	encoder genDup
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushFullBlockClosure: fullBlock [
	| numCopied |
	numCopied := fullBlock copiedValues size.
	numCopied timesRepeat: [ stack pop ].
	stack push.
	encoder
		genPushFullClosure: (self literalIndexOf: fullBlock compiledBlock)
		numCopied: numCopied
		receiverOnStack: false
		outerContextNeeded: fullBlock outerContextNeeded
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushInstVar: instVarIndex [
	stack push.
	forceLongForm
		ifTrue: [ encoder genPushInstVarLong: instVarIndex - 1 ]
		ifFalse: [ encoder genPushInstVar: instVarIndex - 1 ]
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushLiteral: object [
	stack push.
	"identity includes else 0.0 = 0 whereas it's a Float"
	(self encoderClass specialLiterals identityIncludes: object) ifTrue: [
		^ encoder genPushSpecialLiteral: object ].
	encoder genPushLiteral: (self literalIndexOf: object)
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushLiteralVariable: object [
	stack push.
	encoder genPushLiteralVar: (self literalIndexOf: object)
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushNewArray: size [
	stack push.
	encoder genPushNewArray: size
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushReceiver [
	stack push.
	encoder genPushReceiver
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	stack push.
	encoder genPushRemoteTemp: tempIndex - 1 inVectorAt: tempVectorIndex - 1
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushTemp: index [
	stack push.
	encoder genPushTemp: index - 1
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushThisContext [

	stack push.
	encoder genPushThisContext
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> pushThisProcess [

	stack push.
	encoder genPushThisProcess
]

{ #category : 'private' }
OCIRBytecodeGenerator >> quickMethodPrim [

	| index |
	self numArgs = 0 ifFalse: [^ 0].
	lastSpecialReturn ifNil: [^ 0].
	(seqBytes  size <= 2) ifFalse: [^ 0].
	"this is for ruling out the case in which the structure is the same as a quick return
	but with and invalid special literal."
	((literals size = 1) and: [ (encoderClass quickPrimSpecialConstants identityIncludes: literals first) not and: [ lastSpecialReturn selector = #returnConstant: ] ] )
		ifTrue: [^ 0].
	lastSpecialReturn selector == #returnReceiver
		ifTrue: [^256].
	lastSpecialReturn selector == #returnConstant:
		 ifTrue: [^(index := encoderClass quickPrimSpecialConstants identityIndexOf: lastSpecialReturn argument) > 0
					ifTrue: [256 + index] ifFalse: [0]].
	lastSpecialReturn selector == #returnInstVar:
		ifTrue: [^forceLongForm
					ifTrue: [0]  "when compiling long bytecodes for Contexts, do not do quick return either"
					ifFalse: [263 + lastSpecialReturn argument]]
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> returnConstant: constant [
	bytes ifEmpty: [ lastSpecialReturn := Message selector: #returnConstant: argument: constant ].
	(#(nil true false) includes: constant) ifTrue:
		[ ^ encoder genReturnSpecialLiteral:constant ].
	self pushLiteral: constant.
	self returnTop
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> returnInstVar: index [
	bytes ifEmpty: [
		lastSpecialReturn := Message selector: #returnInstVar: argument: index].
	self pushInstVar: index.
	self returnTop
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> returnReceiver [
	bytes ifEmpty: [ lastSpecialReturn := Message selector: #returnReceiver].
	encoder genReturnReceiver
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> returnTop [
	stack pop.
	encoder genReturnTop
]

{ #category : 'private' }
OCIRBytecodeGenerator >> saveLastJump: message [
	jumps at: currentSeqId put: {bytes size. message}
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> send: selector [

	| nArgs |
	nArgs := selector numArgs.
	stack pop: nArgs.
	(compilationContext optionOptimiseSpecialSends and: [
		 self encoderClass specialSelectors includes: selector ]) ifTrue: [
		^ encoder
			  genSendSpecial:
			  (self encoderClass specialSelectors indexOf: selector)
			  numArgs: nArgs ].
	encoder genSend: (self literalIndexOf: selector) numArgs: nArgs
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> send: selector toSuperOf: behavior [
	| index nArgs |
	nArgs := selector numArgs.
	stack pop: nArgs.
	index := self literalIndexOf: selector.

	inBlock
		ifTrue: [
			"we need to use a copy of the binding as else 'references to' will find this method"
			encoder genPushLiteralVar: (self literalIndexOf: behavior binding copy).
			encoder genSendDirectedSuper: index numArgs: nArgs ]
		ifFalse: [
			self addLastLiteral: behavior binding.
			encoder genSendSuper: index numArgs: nArgs ]
]

{ #category : 'results' }
OCIRBytecodeGenerator >> spurVMHeader: literalsSize [
	^ (CompiledMethod headerFlagForEncoder: self encoderClass) +
		(self numArgs bitShift: 24) +
		( self numTemps bitShift: 18) +
		literalsSize +
		(self hasPrimitive asBit bitShift: 16)
]

{ #category : 'results' }
OCIRBytecodeGenerator >> stackFrameSize [
	^ (stacks collect: [:s | s length]) max
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storeInstVar: index [
	forceLongForm
		ifTrue: [ encoder genStoreInstVarLong: index - 1 ]
		ifFalse: [ encoder genStoreInstVar: index - 1 ]
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storeIntoLiteralVariable: object [
	encoder genStoreLiteralVar: (self literalIndexOf: object)
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storePopInstVar: index [
	stack pop.
	forceLongForm
		ifTrue: [ encoder genStorePopInstVarLong: index - 1 ]
		ifFalse: [ encoder genStorePopInstVar: index - 1 ]
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storePopIntoLiteralVariable: assoc [

	encoder genStorePopLiteralVar: (self literalIndexOf: assoc).
	stack pop
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	stack pop.
	encoder genStorePopRemoteTemp: tempIndex - 1 inVectorAt: tempVectorIndex - 1
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storePopTemp: index [
	stack pop.
	encoder genStorePopTemp: index - 1
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storeRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	encoder genStoreRemoteTemp: tempIndex - 1 inVectorAt: tempVectorIndex - 1
]

{ #category : 'instructions' }
OCIRBytecodeGenerator >> storeTemp: index [
	encoder genStoreTemp: index - 1
]

{ #category : 'private' }
OCIRBytecodeGenerator >> updateJump: seqId [
	"Recalculate final jump bytecodes.  Return true if jump bytecodes SIZE has changed, otherwise return false"
	| pair s1 |
	pair := jumps at: seqId.
	pair ifNil: [^ false].  "no jump, a return"
	bytes := seqBytes  at: seqId.
	s1 := bytes size.
	bytes removeLast: (bytes size - pair first).
	pair last sendTo: self.
	^ s1 ~= bytes size
]

{ #category : 'private' }
OCIRBytecodeGenerator >> updateJumpOffsets [
	[ orderSeq
		inject: false
		into: [ :changed :seqId | (self updateJump: seqId) or: [ changed ]]
	] whileTrue
]

{ #category : 'results' }
OCIRBytecodeGenerator >> updateOuterCodeBlocks: code [
	"Add back pointer from compiled block to outer code for all blocks referenced from the literals"

	| blocksToUpdate |
	"Only do that for blocks the compiler created, where the outerCode is not yet set"
	blocksToUpdate := code literals select: [ :each |
		                  each isEmbeddedBlock and: [ each outerCode isNil ] ].
	blocksToUpdate do: [ :each | each outerCode: code ]
]
