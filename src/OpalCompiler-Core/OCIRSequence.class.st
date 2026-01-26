"
A sequence is corresponds to a block in the control flow graph.
In an IRMethod a sequence is sorted by its orderNumber.
"
Class {
	#name : 'OCIRSequence',
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
OCIRSequence class >> orderNumber: aNumber [
	^self new orderNumber: aNumber
]

{ #category : 'copying' }
OCIRSequence >> , otherCollection [
	^sequence, otherCollection
]

{ #category : 'optimizing' }
OCIRSequence >> absorbJumpToSingleInstr: alreadySeen [
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
OCIRSequence >> accept: aVisitor [
	^ aVisitor visitSequence: self
]

{ #category : 'adding' }
OCIRSequence >> add: anInstruction [

	sequence add: anInstruction.
	anInstruction sequence: self.
	^anInstruction
]

{ #category : 'adding' }
OCIRSequence >> add: instr after: another [

	sequence add: instr after: another.
	instr sequence: self.
	^instr
]

{ #category : 'adding' }
OCIRSequence >> add: instr before: another [
	sequence add: instr before: another.
	instr sequence: self.
	^instr
]

{ #category : 'adding' }
OCIRSequence >> addAll: aCollection [
	^sequence addAll: aCollection
]

{ #category : 'adding' }
OCIRSequence >> addAllFirst: aCollection [
	^sequence addAllFirst: aCollection
]

{ #category : 'adding' }
OCIRSequence >> addInstructions: aCollection [

	^aCollection do: [:instr | self add: instr]
]

{ #category : 'adding' }
OCIRSequence >> addInstructions: aCollection after: anInstruction [

	^aCollection reverseDo: [:instr | self add: instr after: anInstruction]
]

{ #category : 'adding' }
OCIRSequence >> addInstructions: aCollection before: anInstruction [

	aCollection do: [:instr | self add: instr before: anInstruction]
]

{ #category : 'adding' }
OCIRSequence >> addLast: anInstruction [
	^self add: anInstruction
]

{ #category : 'accessing' }
OCIRSequence >> after: o [
	^sequence after: o
]

{ #category : 'accessing' }
OCIRSequence >> at: index [
	^sequence at: index
]

{ #category : 'inspector' }
OCIRSequence >> children [
	^sequence
]

{ #category : 'enumerating' }
OCIRSequence >> contains: aBlock [
	^sequence contains: aBlock
]

{ #category : 'enumerating' }
OCIRSequence >> detect: aBlock [
	^sequence detect: aBlock
]

{ #category : 'enumerating' }
OCIRSequence >> do: aBlock [
	^sequence do: aBlock
]

{ #category : 'accessing' }
OCIRSequence >> first [
	^sequence first
]

{ #category : 'testing' }
OCIRSequence >> ifEmpty: aBlock [
	^sequence ifEmpty: aBlock
]

{ #category : 'testing' }
OCIRSequence >> ifNotEmpty: aBlock [
	^sequence ifNotEmpty: aBlock
]

{ #category : 'initialization' }
OCIRSequence >> initialize [
	super initialize.
	sequence := OrderedCollection new
]

{ #category : 'successor sequences' }
OCIRSequence >> instructionsDo: aBlock [

	^self withAllSuccessorsDo: [:seq | seq do: aBlock]
]

{ #category : 'successor sequences' }
OCIRSequence >> instructionsForDecompiling [

	| irInstructions |
	irInstructions := OrderedCollection new.
	self withNonBodySuccessorsDo: [:seq | seq do: [:bc | irInstructions add: bc]].
	^irInstructions
]

{ #category : 'testing' }
OCIRSequence >> isEmpty [
	^sequence isEmpty
]

{ #category : 'testing' }
OCIRSequence >> isNotEmpty [
	^sequence isNotEmpty
]

{ #category : 'accessing' }
OCIRSequence >> last [
	^sequence last
]

{ #category : 'accessing' }
OCIRSequence >> method [

	^method
]

{ #category : 'accessing' }
OCIRSequence >> method: aIRMethod [

	method := aIRMethod
]

{ #category : 'successor sequences' }
OCIRSequence >> nextSequence [

	| sequences i |
	sequences := self withAllSuccessors.
	i := sequences findFirst: [:seq | seq orderNumber = self orderNumber].
	(i = 0 or: [i = sequences size]) ifTrue: [^ nil].
	^ sequences at: i + 1
]

{ #category : 'successor sequences' }
OCIRSequence >> nonBodySuccessorSequences [

	sequence isEmpty ifTrue: [^ #()].
	^ sequence last nonBodySuccessorSequences
]

{ #category : 'accessing' }
OCIRSequence >> orderNumber [
	"Sequences are sorted by this number"

	^ orderNumber
]

{ #category : 'accessing' }
OCIRSequence >> orderNumber: num [
	"Sequences are sorted by this number"
	orderNumber := num
]

{ #category : 'printing' }
OCIRSequence >> printOn: stream [

	stream nextPutAll: 'an '.
	self class printOn: stream.
	stream space.
	stream nextPut: $(.
	self orderNumber printOn: stream.
	stream nextPut: $)
]

{ #category : 'replacing' }
OCIRSequence >> remove: aNode [
	aNode sequence: nil.
	sequence remove: aNode ifAbsent: [self error]
]

{ #category : 'removing' }
OCIRSequence >> removeFirst [
	^sequence removeFirst
]

{ #category : 'removing' }
OCIRSequence >> removeLast [
	^sequence removeLast
]

{ #category : 'removing' }
OCIRSequence >> removeLast: n [
	^sequence removeLast: n
]

{ #category : 'replacing' }
OCIRSequence >> replaceNode: aNode withNode: anotherNode [

	| index |
	index := sequence indexOf: aNode ifAbsent: [ self error ].
	sequence at: index put: (anotherNode sequence: self)
]

{ #category : 'replacing' }
OCIRSequence >> replaceNode: aNode withNodes: aCollection [

	self addInstructions: aCollection before: aNode.
	sequence remove: aNode ifAbsent: [self error]
]

{ #category : 'enumerating' }
OCIRSequence >> reverseDo: aBlock [
	^sequence reverseDo: aBlock
]

{ #category : 'enumerating' }
OCIRSequence >> select: aBlock [
	^sequence select: aBlock
]

{ #category : 'accessing' }
OCIRSequence >> sequence [
	^sequence
]

{ #category : 'accessing' }
OCIRSequence >> size [
	^sequence size
]

{ #category : 'inspector' }
OCIRSequence >> sourceInterval [
	^self sequence first sourceInterval first to: self sequence last sourceInterval last
]

{ #category : 'inspector' }
OCIRSequence >> sourceNode [
	"we should do better here"
	^OCSequenceNode new
]

{ #category : 'manipulating' }
OCIRSequence >> splitAfter: instruction [

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
	self add: (OCIRJump new destination: newSeq; bytecodeIndex: (instruction bytecodeIndex + 1)).
	^ newSeq
]

{ #category : 'successor sequences' }
OCIRSequence >> successorSequences [

	sequence isEmpty ifTrue: [^ #()].
	^ sequence last successorSequences
]

{ #category : 'successor sequences' }
OCIRSequence >> withAllSuccessors [
	"Return me and all my successors sorted by sequence orderNumber"

	| list |
	list := OrderedCollection new: 20.
	self withAllSuccessorsDo: [:seq | list add: seq].
	^ list asSortedCollection: [:x :y | x orderNumber <= y orderNumber]
]

{ #category : 'successor sequences' }
OCIRSequence >> withAllSuccessorsDo: block [
	"Iterate over me and all my successors only once"

	self withAllSuccessorsDo: block alreadySeen: IdentitySet new
]

{ #category : 'successor sequences' }
OCIRSequence >> withAllSuccessorsDo: block alreadySeen: set [
	"Iterate over me and all my successors only once"

	(set includes: self) ifTrue: [^ self].
	set add: self.
	block value: self.
	self successorSequences do: [:seq |
		seq ifNotNil: [seq withAllSuccessorsDo: block alreadySeen: set]]
]

{ #category : 'successor sequences' }
OCIRSequence >> withNonBodySuccessorsDo: block [
	"Iterate over me and all my successors only once"

	self withNonBodySuccessorsDo: block alreadySeen: IdentitySet new
]

{ #category : 'successor sequences' }
OCIRSequence >> withNonBodySuccessorsDo: block alreadySeen: set [
	"Iterate over me and all my successors only once"

	(set includes: self) ifTrue: [^ self].
	set add: self.
	block value: self.
	self nonBodySuccessorSequences do: [:seq |
		seq ifNotNil: [seq withNonBodySuccessorsDo: block alreadySeen: set]]
]
