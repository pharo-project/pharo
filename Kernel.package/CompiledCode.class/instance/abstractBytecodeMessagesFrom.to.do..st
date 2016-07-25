scanning
abstractBytecodeMessagesFrom: startpc to: endpc do: aBlock
	"Evaluate aBlock with the sequence of abstract bytecodes from startpc through endpc in the receiver"
	| scanner |
	scanner := InstructionStream new method: self pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| aBlock value: ex message]]

	"| m msgs |
	 msgs := OrderedCollection new.
	 (m := CompiledMethod >> #abstractBytecodeMessagesFrom:to:)
		abstractBytecodeMessagesFrom: m initialPC
		to: m endPC
		do: [:msg| msgs add: msg selector].
	 msgs"