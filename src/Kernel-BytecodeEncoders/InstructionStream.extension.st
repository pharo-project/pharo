Extension { #name : 'InstructionStream' }

{ #category : '*Kernel-BytecodeEncoders' }
InstructionStream >> secondByte [
	"Answer the second byte of the current bytecode."

	^self method at: pc + 1
]
