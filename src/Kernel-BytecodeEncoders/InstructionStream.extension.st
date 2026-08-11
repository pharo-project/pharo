Extension { #name : 'InstructionStream' }

{ #category : '*Kernel-BytecodeEncoders' }
InstructionStream >> secondByte [
	"Answer the second byte of the current bytecode."

	^ self method at: self pc + 1
]

{ #category : '*Kernel-BytecodeEncoders' }
InstructionStream >> selectorToSendOrSelf [
	"If this instruction is a send, answer the selector, otherwise answer self."
 
	^ self method encoderClass selectorToSendOrItselfFor: self in: self method at: pc
]
