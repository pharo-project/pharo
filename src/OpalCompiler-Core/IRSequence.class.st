"
A sequence is corresponds to a block in the control flow graph.
"
Class {
	#name : 'IRSequence',
	#superclass : 'Object',
	#instVars : [
		'sequence',
		'orderNumber',
		'method'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'instance creation' }
IRSequence class >> orderNumber: aNumber [
	^self new orderNumber: aNumber
]

{ #category : 'copying' }
IRSequence >> , otherCollection [
	^sequence, otherCollection
]

{ #category : 'optimizing' }
IRSequence >> absorbJumpToSingleInstr: alreadySeen [
	"Collapse jumps to single return instructions into caller"

	| seqs seq |
	(alreadySeen includes: self) ifTrue: [^ self].
	alreadySeen add: self.
	[ (seqs := self successorSequences) size = 1  "unconditional jump..."
	   and: [((seq := seqs first) size = 1)  "...to single instruction..."
	    and: [seq successorSequences size < 2
		and: [self last isBlockReturnTop not
		  and: [((seq sequence size = 1) and: [(seq sequence first isBlockReturnTop) or: [seq sequence first isReturn]]) not]]]]"...but don't collapse conditional jumps so their otherwiseSequences can stay right after them"
	] whileTrue: [ "replace goto with single instruction"
		self removeLast.
		seq do: [:instr |  self add: instr copy].
	].

	seqs do: [:instrs | instrs ifNotNil: [:i | i absorbJumpToSingleInstr: alreadySeen]]
]

{ #category : 'visiting' }
IRSequence >> accept: aVisitor [
	^ aVisitor visitSequence: self
]

{ #category : 'adding' }
IRSequence >> add: anInstruction [

	sequence add: anInstruction.
	anInstruction sequence: self.
	^anInstruction
]

{ #category : 'adding' }
IRSequence >> add: instr after: another [

	sequence add: instr after: another.
	instr sequence: self.
	^instr
]

{ #category : 'adding' }
IRSequence >> add: instr before: another [
	sequence add: instr before: another.
	instr sequence: self.
	^instr
]

{ #category : 'adding' }
IRSequence >> addAll: aCollection [
	^sequence addAll: aCollection
]

{ #category : 'adding' }
IRSequence >> addAllFirst: aCollection [
	^sequence addAllFirst: aCollection
]

{ #category : 'adding' }
IRSequence >> addInstructions: aCollection [

	^aCollection do: [:instr | self add: instr]
]

{ #category : 'adding' }
IRSequence >> addInstructions: aCollection after: anInstruction [

	^aCollection reverseDo: [:instr | self add: instr after: anInstruction]
]

{ #category : 'adding' }
IRSequence >> addInstructions: aCollection before: anInstruction [

	aCollection do: [:instr | self add: instr before: anInstruction]
]

{ #category : 'adding' }
IRSequence >> addLast: anInstruction [
	^self add: anInstruction
]

{ #category : 'accessing' }
IRSequence >> after: o [
	^sequence after: o
]

{ #category : 'accessing' }
IRSequence >> at: index [
	^sequence at: index
]

{ #category : 'inspector' }
IRSequence >> children [
	^sequence
]

{ #category : 'enumerating' }
IRSequence >> contains: aBlock [
	^sequence contains: aBlock
]

{ #category : 'enumerating' }
IRSequence >> detect: aBlock [
	^sequence detect: aBlock
]

{ #category : 'enumerating' }
IRSequence >> do: aBlock [
	^sequence do: aBlock
]

{ #category : 'accessing' }
IRSequence >> first [
	^sequence first
]

{ #category : 'testing' }
IRSequence >> ifEmpty: aBlock [
	^sequence ifEmpty: aBlock
]

{ #category : 'testing' }
IRSequence >> ifNotEmpty: aBlock [
	^sequence ifNotEmpty: aBlock
]

{ #category : 'initialization' }
IRSequence >> initialize [
	sequence := OrderedCollection new
]

{ #category : 'successor sequences' }
IRSequence >> instructionsDo: aBlock [

	^self withAllSuccessorsDo: [:seq | seq do: aBlock]
]

{ #category : 'successor sequences' }
IRSequence >> instructionsForDecompiling [

	| irInstructions |
	irInstructions := OrderedCollection new.
	self withNonBodySuccessorsDo: [:seq | seq do: [:bc | irInstructions add: bc]].
	^irInstructions
]

{ #category : 'testing' }
IRSequence >> isEmpty [
	^sequence isEmpty
]

{ #category : 'testing' }
IRSequence >> isNotEmpty [
	^sequence isNotEmpty
]

{ #category : 'accessing' }
IRSequence >> last [
	^sequence last
]

{ #category : 'accessing' }
IRSequence >> method [

	^method
]

{ #category : 'accessing' }
IRSequence >> method: aIRMethod [

	method := aIRMethod
]

{ #category : 'successor sequences' }
IRSequence >> nextSequence [

	| sequences i |
	sequences := self withAllSuccessors.
	i := sequences findFirst: [:seq | seq orderNumber = self orderNumber].
	(i = 0 or: [i = sequences size]) ifTrue: [^ nil].
	^ sequences at: i + 1
]

{ #category : 'successor sequences' }
IRSequence >> nonBodySuccessorSequences [

	sequence isEmpty ifTrue: [^ #()].
	^ sequence last nonBodySuccessorSequences
]

{ #category : 'testing' }
IRSequence >> notEmpty [
	self deprecated: 'Use #isNotEmpty instead.' transformWith: '`@rcv notEmpty' -> '`@rcv isNotEmpty'.
	^self isNotEmpty
]

{ #category : 'accessing' }
IRSequence >> orderNumber [
	"Sequences are sorted by this number"

	^ orderNumber
]

{ #category : 'accessing' }
IRSequence >> orderNumber: num [
	"Sequences are sorted by this number"
	orderNumber := num
]

{ #category : 'printing' }
IRSequence >> printOn: stream [

	stream nextPutAll: 'an '.
	self class printOn: stream.
	stream space.
	stream nextPut: $(.
	self orderNumber printOn: stream.
	stream nextPut: $)
]

{ #category : 'replacing' }
IRSequence >> remove: aNode [
	aNode sequence: nil.
	sequence remove: aNode ifAbsent: [self error]
]

{ #category : 'removing' }
IRSequence >> removeFirst [
	^sequence removeFirst
]

{ #category : 'removing' }
IRSequence >> removeLast [
	^sequence removeLast
]

{ #category : 'removing' }
IRSequence >> removeLast: n [
	^sequence removeLast: n
]

{ #category : 'replacing' }
IRSequence >> replaceNode: aNode withNode: anotherNode [

	| index |
	index := sequence indexOf: aNode ifAbsent: [ self error ].
	sequence at: index put: (anotherNode sequence: self)
]

{ #category : 'replacing' }
IRSequence >> replaceNode: aNode withNodes: aCollection [

	self addInstructions: aCollection before: aNode.
	sequence remove: aNode ifAbsent: [self error]
]

{ #category : 'enumerating' }
IRSequence >> reverseDo: aBlock [
	^sequence reverseDo: aBlock
]

{ #category : 'enumerating' }
IRSequence >> select: aBlock [
	^sequence select: aBlock
]

{ #category : 'accessing' }
IRSequence >> sequence [
	^sequence
]

{ #category : 'accessing' }
IRSequence >> size [
	^sequence size
]

{ #category : 'inspector' }
IRSequence >> sourceInterval [
	^self sequence first sourceInterval first to: self sequence last sourceInterval last
]

{ #category : 'inspector' }
IRSequence >> sourceNode [
	"we should do better here"
	^ASTSequenceNode new
]

{ #category : 'manipulating' }
IRSequence >> splitAfter: instruction [

	| newSeq index next |
	next := self nextSequence.
	next := next
		ifNil: [self orderNumber + 1]
		ifNotNil: [(next orderNumber + self orderNumber) / 2].
	newSeq := self class new orderNumber: next.
	newSeq method: self method.
	"Split after instruction"
	index := sequence indexOf: instruction.
	(sequence last: sequence size - index) do: [:instr | newSeq add: instr].
	sequence := sequence first: index.
	self flag: 'The jump inherits the bytecode index from the instruction where it was split. Check if this value is correct.'.
	self add: (IRJump new destination: newSeq; bytecodeIndex: (instruction bytecodeIndex + 1)).
	^ newSeq
]

{ #category : 'successor sequences' }
IRSequence >> successorSequences [

	sequence isEmpty ifTrue: [^ #()].
	^ sequence last successorSequences
]

{ #category : 'successor sequences' }
IRSequence >> withAllSuccessors [
	"Return me and all my successors sorted by sequence orderNumber"

	| list |
	list := OrderedCollection new: 20.
	self withAllSuccessorsDo: [:seq | list add: seq].
	^ list asSortedCollection: [:x :y | x orderNumber <= y orderNumber]
]

{ #category : 'successor sequences' }
IRSequence >> withAllSuccessorsDo: block [
	"Iterate over me and all my successors only once"

	self withAllSuccessorsDo: block alreadySeen: IdentitySet new
]

{ #category : 'successor sequences' }
IRSequence >> withAllSuccessorsDo: block alreadySeen: set [
	"Iterate over me and all my successors only once"

	(set includes: self) ifTrue: [^ self].
	set add: self.
	block value: self.
	self successorSequences do: [:seq |
		seq ifNotNil: [seq withAllSuccessorsDo: block alreadySeen: set]]
]

{ #category : 'successor sequences' }
IRSequence >> withNonBodySuccessorsDo: block [
	"Iterate over me and all my successors only once"

	self withNonBodySuccessorsDo: block alreadySeen: IdentitySet new
]

{ #category : 'successor sequences' }
IRSequence >> withNonBodySuccessorsDo: block alreadySeen: set [
	"Iterate over me and all my successors only once"

	(set includes: self) ifTrue: [^ self].
	set add: self.
	block value: self.
	self nonBodySuccessorSequences do: [:seq |
		seq ifNotNil: [seq withNonBodySuccessorsDo: block alreadySeen: set]]
]
