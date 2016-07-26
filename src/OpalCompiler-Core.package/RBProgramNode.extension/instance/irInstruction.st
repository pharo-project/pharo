*opalcompiler-core
irInstruction
	"search for the first ir instruction that I generate"
	self methodNode ir sourceNode == self  ifTrue: [^self methodNode ir].
	^self methodNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
	
