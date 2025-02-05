"
I am an instruction in the IR (intermediate representation) language. The IR serves as the intermediary between the Pharo syntax (AST) and the bytecode language. The IR is generic and simple consisting of just twelve instructions.  

They are:

-	goto: labelNum
-	if: boolean goto: labelNum1 otherwise: labelNum2
-	label: labelNum
-	popTop
-	pushDup
-	pushLiteral: object
-	pushTemp: name
-	blockReturn
-	returnTop
-	send: selector
-	send: selector toSuperOf: behavior
-	storeTemp: name


Each instruction is reified as an instance of one of my subclasses and grouped by basic blocks (`IRSequence`) into an `IRMethod`.  `IRInterpreter` visits each instruction in an `IRMethod` responding to the above instruction messages sent to it.

"
Class {
	#name : 'OCIRInstruction',
	#superclass : 'Object',
	#instVars : [
		'sourceNode',
		'bytecodeIndex',
		'sequence'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'instance creation' }
OCIRInstruction class >> blockReturnTop [
	^ OCIRBlockReturnTop new
]

{ #category : 'instance creation' }
OCIRInstruction class >> createTempVectorNamed: aTempVectorName withVars: anArray [

	^ OCIRTempVector new
		name: aTempVectorName;
		vars: anArray;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> popTop [
	^ OCIRPop new
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushConsArray: aSize [
	^OCIRPushArray new
		size: aSize;
		cons: true;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushDup [

	^ OCIRPushDup new
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues [
	^OCIRPushFullClosure new
			copiedValues: copiedValues;
			compiledBlock: compiledBlock;
			yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues outerContextNeeded: aBoolean [
	^OCIRPushFullClosure new
			copiedValues: copiedValues;
			compiledBlock: compiledBlock;
			outerContextNeeded: aBoolean
			yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushInstVar: index [

	^ OCIRPushInstVar new
			index:  index;
			yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushLiteral: object [

	^ OCIRPushLiteral new
		literal: object
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushLiteralVariable: object [

	^ OCIRPushLiteralVariable new
		association: object;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushNewArray: aSize [

	^OCIRPushArray new
		size: aSize;
		cons: false;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushReceiver [
	^ OCIRPushReceiver new
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushRemoteTemp: aName inVectorAt: nameOfVector [
	^ OCIRPushRemoteTemp new
		name: aName;
		tempVectorName: nameOfVector;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushTemp: aName [

	^ OCIRPushTemp new
		name: aName;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushThisContext [
	^ OCIRPushThisContext new
]

{ #category : 'instance creation' }
OCIRInstruction class >> pushThisProcess [
	^ OCIRPushThisProcess new
]

{ #category : 'instance creation' }
OCIRInstruction class >> returnTop [

	^ OCIRReturn new
]

{ #category : 'instance creation' }
OCIRInstruction class >> send: selector [

	^ OCIRSend new
		selector: selector
]

{ #category : 'instance creation' }
OCIRInstruction class >> send: selector toSuperOf: behavior [

	behavior ifNil: [self error: 'super of nil does not exist'].
	^ OCIRSend new
		selector: selector;
		superOf: behavior;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> storeInstVar: index [
	^ OCIRStoreInstVar new
		index: index;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> storeIntoLiteralVariable: object [

	^ OCIRStoreLiteralVariable new
		association: object;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> storeRemoteTemp: aName inVectorAt: nameOfVector [
	^ OCIRStoreRemoteTemp new
		name: aName;
		tempVectorName: nameOfVector;
		yourself
]

{ #category : 'instance creation' }
OCIRInstruction class >> storeTemp: aName [
	^ OCIRStoreTemp new
		name: aName;
		yourself
]

{ #category : 'visiting' }
OCIRInstruction >> accept: aVisitor [
	self subclassResponsibility
]

{ #category : 'adding' }
OCIRInstruction >> addInstructionsAfter: aCollection [
	sequence addInstructions: aCollection after: self
]

{ #category : 'adding' }
OCIRInstruction >> addInstructionsBefore: aCollection [
	sequence addInstructions: aCollection before: self
]

{ #category : 'mapping' }
OCIRInstruction >> bytecodeIndex [

	^ bytecodeIndex
]

{ #category : 'mapping' }
OCIRInstruction >> bytecodeIndex: index [

	bytecodeIndex := index
]

{ #category : 'mapping' }
OCIRInstruction >> bytecodeOffset [
	| startpc |
	startpc := self method compiledMethod initialPC.
	self bytecodeIndex ifNil: [^startpc].
	^self bytecodeIndex + startpc - 1
]

{ #category : 'testing' }
OCIRInstruction >> canBeQuickReturn [
	^ false
]

{ #category : 'inspector' }
OCIRInstruction >> children [
	^#()
]

{ #category : 'replacing' }
OCIRInstruction >> delete [
	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence remove: self
]

{ #category : 'testing' }
OCIRInstruction >> isBlockReturnTop [

	^false
]

{ #category : 'testing' }
OCIRInstruction >> isGoto [
	"is unconditional jump"

	^ false
]

{ #category : 'testing' }
OCIRInstruction >> isIf [

	^ false
]

{ #category : 'testing' }
OCIRInstruction >> isInstVarAccess [

	^false
]

{ #category : 'testing' }
OCIRInstruction >> isJump [
	"goto or if"

	^false
]

{ #category : 'testing' }
OCIRInstruction >> isLiteralVariable [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isPop [

	^ false
]

{ #category : 'testing' }
OCIRInstruction >> isPushLiteral [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isPushLiteral: valueTest [

	^ false
]

{ #category : 'testing' }
OCIRInstruction >> isRead [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isRemovableByPop [

	^ false
]

{ #category : 'testing' }
OCIRInstruction >> isReturn [

	^ false
]

{ #category : 'testing' }
OCIRInstruction >> isSelf [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isSend [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isStore [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isTemp [
	^false
]

{ #category : 'testing' }
OCIRInstruction >> isTempVector [
	^false
]

{ #category : 'accessing' }
OCIRInstruction >> method [
	^sequence method
]

{ #category : 'accessing' }
OCIRInstruction >> nonBodySuccessorSequences [
	^self successorSequences
]

{ #category : 'printing' }
OCIRInstruction >> printOn: aStream [
	OCIRPrinterVisitor new
			stream: aStream;
			visitNode: self
]

{ #category : 'replacing' }
OCIRInstruction >> replaceNode: aNode withNode: anotherNode [
	self error: 'I don''t store other nodes'
]

{ #category : 'replacing' }
OCIRInstruction >> replaceWith: aNode [
	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence replaceNode: self withNode: aNode
]

{ #category : 'replacing' }
OCIRInstruction >> replaceWithInstructions: aCollection [

	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence replaceNode: self withNodes: aCollection
]

{ #category : 'accessing' }
OCIRInstruction >> sequence [
	^sequence
]

{ #category : 'accessing' }
OCIRInstruction >> sequence: aSeq [
	sequence := aSeq
]

{ #category : 'inspector' }
OCIRInstruction >> sourceInterval [
	^self sourceNode sourceInterval
]

{ #category : 'mapping' }
OCIRInstruction >> sourceNode [

	^ sourceNode
]

{ #category : 'mapping' }
OCIRInstruction >> sourceNode: parseNode [

	sourceNode := parseNode
]

{ #category : 'accessing' }
OCIRInstruction >> successorSequences [
	"sent to last instruction in sequence which is expected to be a jump and return instruction"

	^ #()
]

{ #category : 'testing' }
OCIRInstruction >> transitionsToNextSequence [
	"Does the current instruction intrinsically know how to transition to the next sequence?"
	^ self isJump or: [self isReturn]
]
