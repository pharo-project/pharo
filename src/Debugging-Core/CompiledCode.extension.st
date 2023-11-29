Extension { #name : 'CompiledCode' }

{ #category : '*Debugging-Core' }
CompiledCode >> abstractBytecodeMessageAt: pc [
	"Answer the abstract bytecode message at pc in the receiver."
	^[(InstructionStream new method: self pc: pc) interpretNextInstructionFor: nil]
		on: MessageNotUnderstood
		do: [:ex| ex message]
]

{ #category : '*Debugging-Core' }
CompiledCode >> abstractBytecodeMessagesFrom: startpc to: endpc do: aBlock [
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
]

{ #category : '*Debugging-Core' }
CompiledCode >> bytecodeSizeForPc: pc [
	"Returns the size of the bytecode at the given pc"

	^ self encoderClass bytecodeSize: (self at: pc)
]

{ #category : '*Debugging-Core' }
CompiledCode >> pcPreviousTo: pc [
	| scanner prevPc |
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	[scanner pc < pc] whileTrue: [
		prevPc := scanner pc.
		scanner advanceToFollowingPc ].
	^prevPc
]

{ #category : '*Debugging-Core' }
CompiledCode >> rangeForPC: aPC [	
 	"return which code to hightlight in the debugger"		
 	^(self sourceNodeForPC: aPC) debugHighlightRange
]

{ #category : '*Debugging-Core' }
CompiledCode >> symbolicBytecodes [
	"Answer Collection that contains of all the byte codes in a method as an instance of SymbolicInstruction"

	^SymbolicBytecodeBuilder decode: self
]
