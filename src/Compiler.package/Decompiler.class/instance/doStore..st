instruction decoding
doStore: stackOrBlock
	"Only called internally, not from InstructionStream. StackOrBlock is stack
	for store, statements for storePop."

	| var expr |
	var := stack removeLast.
	expr := stack removeLast.
	stackOrBlock addLast: (expr == ArgumentFlag
		ifTrue: [var]
		ifFalse: [constructor codeAssignTo: var value: expr])