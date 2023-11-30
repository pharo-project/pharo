Extension { #name : 'InstructionStream' }

{ #category : '*OpalCompiler-Tests' }
InstructionStream >> decodeNextInstruction [
	"Return the next bytecode instruction as a message that an InstructionClient would understand.  This advances the pc by one instruction."

	^ self interpretNextInstructionFor: MessageCatcher new
]

{ #category : '*OpalCompiler-Tests' }
InstructionStream >> peekInstruction [
	"Return the next bytecode instruction as a message that an InstructionClient would understand.  The pc remains unchanged."

	| currentPc instruction |
	currentPc := self pc.
	instruction := self decodeNextInstruction.
	self pc: currentPc.
	^ instruction
]
