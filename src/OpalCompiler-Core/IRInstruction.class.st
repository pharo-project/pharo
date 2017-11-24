"
I am an instruction in the IR (intermediate representation) language.  The IR serves as the intermediary between the Smalltalk language and the bytecode language.  It is easier to optimize and translate to/from this language than it is to optimize/translate directly from Smalltalk to bytecodes.  The IR is generic and simple consisting of just twelve instructions.  They are:

	goto: labelNum
	if: boolean goto: labelNum1 otherwise: labelNum2
	label: labelNum
	popTop
	pushDup
	pushLiteral: object
	pushTemp: name
	blockReturn
	returnTop
	send: selector
	send: selector toSuperOf: behavior
	storeTemp: name

Each instruction is reified as an instance of one of my subclasses and grouped by basic block (IRSequence) into an IRMethod.  IRInterpreter visits each instruction in a IRMethod responding to the above instruction messages sent to it.

"
Class {
	#name : #IRInstruction,
	#superclass : #Object,
	#instVars : [
		'sourceNode',
		'bytecodeIndex',
		'sequence'
	],
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #'instance creation' }
IRInstruction class >> blockReturnTop [
	^ IRBlockReturnTop new
		
]

{ #category : #'instance creation' }
IRInstruction class >> createTempVectorNamed: aTempVectorName withVars: anArray [  

	^ IRTempVector new
		name: aTempVectorName;
		vars: anArray;
		yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> popTop [
	^ IRPop new
]

{ #category : #'instance creation' }
IRInstruction class >> pushClosureCopyCopiedValues: copiedValues args: arguments [
	^IRPushClosureCopy new
			copiedValues: copiedValues;
			arguments: arguments;
			yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> pushConsArray: aSize [ 
	^IRPushArray new
		size: aSize;
		cons: true;
		yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> pushDup [

	^ IRPushDup new
]

{ #category : #'instance creation' }
IRInstruction class >> pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues [
	^IRPushFullClosure new
			copiedValues: copiedValues;
			compiledBlock: compiledBlock;
			yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> pushInstVar: index [

	^ IRPushInstVar new 
			index:  index; 
			yourself
]

{ #category : #'instance creation' }
IRInstruction class >> pushLiteral: object [

	^ IRPushLiteral new
		literal: object
]

{ #category : #'instance creation' }
IRInstruction class >> pushLiteralVariable: object [

	^ IRPushLiteralVariable new
		association: object;
		yourself
	 
]

{ #category : #'instance creation' }
IRInstruction class >> pushNewArray: aSize [
	
	^IRPushArray new
		size: aSize;
		cons: false;
		yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> pushReceiver [
	^ IRPushReceiver new
]

{ #category : #'instance creation' }
IRInstruction class >> pushRemoteTemp: aName inVectorAt: nameOfVector [
	^ IRPushRemoteTemp new
		name: aName;
		tempVectorName: nameOfVector;
		yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> pushTemp: aName [
	aName = 'self' ifTrue: [self error: 'use pushReceiver'].
	^ IRPushTemp new
		name: aName;
		yourself
]

{ #category : #'instance creation' }
IRInstruction class >> pushThisContext [
	^ IRPushThisContext new
]

{ #category : #'instance creation' }
IRInstruction class >> returnTop [

	^ IRReturn new
]

{ #category : #'instance creation' }
IRInstruction class >> send: selector [

	^ IRSend new
		selector: selector
]

{ #category : #'instance creation' }
IRInstruction class >> send: selector toSuperOf: behavior [

	behavior ifNil: [self error: 'super of nil does not exist'].
	^ IRSend new
		selector: selector;
		superOf: behavior;
		yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> storeInstVar: index [
	^ IRStoreInstVar new 
		index: index; 
		yourself
	
]

{ #category : #'instance creation' }
IRInstruction class >> storeIntoLiteralVariable: object [

	^ IRStoreLiteralVariable new
		association: object;
		yourself
]

{ #category : #'instance creation' }
IRInstruction class >> storeRemoteTemp: aName inVectorAt: nameOfVector [
	^ IRStoreRemoteTemp new
		name: aName;
		tempVectorName: nameOfVector;
		yourself.
]

{ #category : #'instance creation' }
IRInstruction class >> storeTemp: aName [
	^ IRStoreTemp new
		name: aName;
		yourself

]

{ #category : #visiting }
IRInstruction >> accept: aVisitor [
	self subclassResponsibility
]

{ #category : #adding }
IRInstruction >> addInstructionsAfter: aCollection [
	sequence addInstructions: aCollection after: self.
]

{ #category : #adding }
IRInstruction >> addInstructionsBefore: aCollection [
	sequence addInstructions: aCollection before: self.
]

{ #category : #mapping }
IRInstruction >> bytecodeIndex [

	^ bytecodeIndex
]

{ #category : #mapping }
IRInstruction >> bytecodeIndex: index [

	bytecodeIndex := index
]

{ #category : #mapping }
IRInstruction >> bytecodeOffset [
	| startpc |
	startpc := self method compiledMethod initialPC.
	self bytecodeIndex ifNil: [^startpc].
	^self bytecodeIndex + startpc - 1.
]

{ #category : #testing }
IRInstruction >> canBeQuickReturn [
	^ false
]

{ #category : #inspector }
IRInstruction >> children [
	^#()
]

{ #category : #replacing }
IRInstruction >> delete [
	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence remove: self.
]

{ #category : #testing }
IRInstruction >> isBlockReturnTop [

	^false 
]

{ #category : #testing }
IRInstruction >> isGoto [
	"is unconditional jump"
	
	^ false 
]

{ #category : #testing }
IRInstruction >> isIf [

	^ false 
]

{ #category : #testing }
IRInstruction >> isInstVarAccess [

	^false.
]

{ #category : #testing }
IRInstruction >> isJump [
	"goto or if"

	^false 
]

{ #category : #testing }
IRInstruction >> isLiteralVariable [
	^false
]

{ #category : #testing }
IRInstruction >> isPop [

	^ false
]

{ #category : #testing }
IRInstruction >> isPushClosureCopy [
	^false
]

{ #category : #testing }
IRInstruction >> isPushLiteral [
	^false
]

{ #category : #testing }
IRInstruction >> isPushLiteral: valueTest [

	^ false
]

{ #category : #testing }
IRInstruction >> isRead [
	^false
]

{ #category : #testing }
IRInstruction >> isReturn [

	^ false 
]

{ #category : #testing }
IRInstruction >> isSelf [
	^false
]

{ #category : #testing }
IRInstruction >> isSend [
	^false.
]

{ #category : #testing }
IRInstruction >> isStore [
	^false
]

{ #category : #testing }
IRInstruction >> isTemp [
	^false
]

{ #category : #testing }
IRInstruction >> isTempVector [
	^false
]

{ #category : #accessing }
IRInstruction >> method [
	^sequence method.
]

{ #category : #accessing }
IRInstruction >> nonBodySuccessorSequences [
	^self successorSequences
]

{ #category : #printing }
IRInstruction >> printOn: aStream [
	IRPrinterV2 new
			stream: aStream;
			visitNode: self
]

{ #category : #replacing }
IRInstruction >> replaceNode: aNode withNode: anotherNode [ 
	self error: 'I don''t store other nodes'
]

{ #category : #replacing }
IRInstruction >> replaceWith: aNode [
	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence replaceNode: self withNode: aNode
]

{ #category : #replacing }
IRInstruction >> replaceWithInstructions: aCollection [ 

	sequence ifNil: [self error: 'This node doesn''t have a sequence'].
	sequence replaceNode: self withNodes: aCollection
]

{ #category : #accessing }
IRInstruction >> sequence [
	^sequence
]

{ #category : #accessing }
IRInstruction >> sequence: aSeq [
	sequence := aSeq
]

{ #category : #inspector }
IRInstruction >> sourceInterval [
	^self sourceNode sourceInterval
]

{ #category : #mapping }
IRInstruction >> sourceNode [

	^ sourceNode
	
]

{ #category : #mapping }
IRInstruction >> sourceNode: parseNode [

	sourceNode := parseNode
	
]

{ #category : #mapping }
IRInstruction >> sourceNodeExecuted [

	^ self sourceNode
	
]

{ #category : #accessing }
IRInstruction >> successorSequences [
	"sent to last instruction in sequence which is expected to be a jump and return instruction"

	^ #()
]

{ #category : #testing }
IRInstruction >> transitionsToNextSequence [
	"Does the current instruction intrinsically know how to transition to the next sequence?"
	^ self isJump or: [self isReturn or: [self isPushClosureCopy ]]
]
