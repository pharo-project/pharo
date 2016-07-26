manipulating
splitAfter: instruction

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