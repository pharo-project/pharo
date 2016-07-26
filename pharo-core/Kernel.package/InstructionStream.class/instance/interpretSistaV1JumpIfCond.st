decoding
interpretSistaV1JumpIfCond
	"If the instruction at pc is a conditional jump, interpret it, advancing
	the pc,
	and answering the jump distance. Otherwise answer nil."
	"184-191	10111 iii				Pop and Jump 0n True iii +1 (i.e., 1 through 8)
	192-199	11000 iii				Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	*	225/E1		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256
	+ Ext B)
	*	238		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B *
	256, where Extend B >= 0)
	*	239		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B *
	256, where Extend B >= 0)"
	| method byte nextpc extB |
	method := self method.
	"consume and compute any extension first."
	extB := 0.
	nextpc := pc.
	"must not advance pc unless this is a jump."
	[byte := self method at: nextpc.
	nextpc := nextpc + 1.
	byte = 225]
		whileTrue: [| extByte |
			extByte := self method at: nextpc.
			nextpc := nextpc + 1.
			extB := (extB = 0
							and: [extByte > 127])
						ifTrue: [extByte - 256]
						ifFalse: [(extB bitShift: 8)
								+ extByte]].
	(byte between: 184 and: 199)
		ifTrue: [pc := nextpc.
			^ (byte bitAnd: 7)
				+ 1].
	(byte between: 238 and: 239)
		ifTrue: [byte := method at: nextpc.
			pc := nextpc + 1.
			^ (extB bitShift: 8)
				+ byte].
	^ nil
