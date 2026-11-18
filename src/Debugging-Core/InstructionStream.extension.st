Extension { #name : 'InstructionStream' }

{ #category : '*Debugging-Core' }
InstructionStream >> addSelectorTo: set [ 
	"If this instruction is a send, add its selector to set."

	| selectorOrSelf |
	(selectorOrSelf := self selectorToSendOrSelf) isSymbol ifTrue:
		[ set add: selectorOrSelf ]
]

{ #category : '*Debugging-Core' }
InstructionStream >> atEnd [

	^ pc > self method endPC
]

{ #category : '*Debugging-Core' }
InstructionStream >> followingBytecode [
	"Answer the bytecode of the following bytecode (different to nextByte)."

	^self method at: self followingPc
]

{ #category : '*Debugging-Core' }
InstructionStream >> interpret [
	"Interpret / evaluate the current method in the current context until the end.
	Use the context as client of the interpretation as well as the decoder of instructions.
	The context is a client that evaluates instructions"
	self interpretWithClient: self
]

{ #category : '*Debugging-Core' }
InstructionStream >> peekByte [
	"Answer the first byte of the current bytecode."

	^self method at: pc
]

{ #category : '*Debugging-Core' }
InstructionStream >> previousPc [

	"Return the PC of the instruction preceding the current one"
	^self method pcPreviousTo: pc
]

{ #category : '*Debugging-Core' }
InstructionStream >> thirdByte [
	"Answer the third byte of the current bytecode."

	^ self method at: self pc + 2
]
