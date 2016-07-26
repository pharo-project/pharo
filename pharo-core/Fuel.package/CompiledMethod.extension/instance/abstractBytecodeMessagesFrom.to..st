*Fuel
abstractBytecodeMessagesFrom: startpc to: endpc
	"Answer an OrderedCollection of the abstract bytecodes sent in the receiver."
	| scanner abstractBytecodes |
	scanner := InstructionStream new method: self pc: startpc.
	"now collect all the bytecode messages in the block."
	abstractBytecodes := OrderedCollection new.
	[scanner pc <= endpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| abstractBytecodes add: ex message]].
	^abstractBytecodes

	"| m | (m := CompiledMethod >> #abstractBytecodeMessagesFrom:to:) abstractBytecodeMessagesFrom: m initialPC to: m endPC"