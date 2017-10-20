instruction decoding
jump: dist
	| index seq instr newSeq seqs |
	index := instructionStream pc + dist.

	dist >= 0 ifTrue: [
		"jump forward"
		^ irBuilder jumpAheadTo: index ].
	"jump to the jump instuction itself"
	dist >= -2 ifTrue: 
		[	irBuilder jumpBackTarget: index.
			irBuilder jumpBackTo: index.
		^self].
	"jump backward"
	seqs := irBuilder ir allSequences.
	seq := seqs findLast: [:s | s notEmpty and: [s first bytecodeIndex <= index]].
	seq := seqs at: seq.
	seq first bytecodeIndex = index
		ifTrue: [ newSeq := seq ]
		ifFalse: [
			instr := seq detect: [:i | (seq after: i) bytecodeIndex = index].
			newSeq := seq splitAfter: instr ].
	irBuilder addJumpBackTarget: index to: newSeq.
	"if we have split the currentSequence of the irBuilder, make sure to set it
	to the newSeq"
	irBuilder currentSequence = seq ifTrue: [irBuilder currentSequence: newSeq].
	irBuilder jumpBackTo: index.

