private-extensions-compiledmethod
abstractBytecodeMessageAt: pc
	<flExtensionOf: #CompiledMethod>
	"Answer the abstract bytecode message at pc in the receiver."
	^[(InstructionStream new method: self pc: pc) interpretNextInstructionFor: nil]
		on: MessageNotUnderstood
		do: [:ex| ex message]