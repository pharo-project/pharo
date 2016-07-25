optimizing
absorbJumpToSingleInstr: alreadySeen
	"Collapse jumps to single return instructions into caller"

	| seqs seq |
	(alreadySeen includes: self) ifTrue: [^ self].
	alreadySeen add: self.
	[ (seqs := self successorSequences) size = 1  "unconditional jump..." 
	   and: [((seq := seqs first) size = 1)  "...to single instruction..."
	    and: [seq successorSequences size < 2
		and: [self last isBlockReturnTop not
		  and: [((seq sequence size = 1) and: [(seq sequence first isBlockReturnTop) or: [seq sequence first isReturn]]) not]]]]"...but don't collapse conditional jumps so their otherwiseSequences can stay right after them"
	] whileTrue: [ "replace goto with single instruction"
		self removeLast.
		seq do: [:instr |  self add: instr copy].
	].

	seqs do: [:instrs | instrs ifNotNil: [:i | i absorbJumpToSingleInstr: alreadySeen]].
