private-extensions-compiledmethod
abstractBytecodeMessagesFrom: startpc to: endpc do: aBlock
	<flExtensionOf: #CompiledMethod>
	"Evaluate aBlock with the sequence of abstract bytecodes from startpc through endpc in the receiver"
	| scanner |
	scanner := InstructionStream new method: self pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| aBlock value: ex message]]