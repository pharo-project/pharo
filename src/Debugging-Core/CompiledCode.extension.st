Extension { #name : 'CompiledCode' }

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
