decoding
interpretSistaV1Jump
	"If the instruction at pc is an unconditional jump, interpret it, advancing
	the pc,
	and answering the jump distance. Otherwise answer nil."
	"176-183	10110 iii				Jump iii + 1 (i.e., 1 through 8)
	*	225/16rE1	11100001	sbbbbbbb	Extend B (Ext B = Ext B prev *
	256 + Ext B)
	*	237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where
	bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
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
	(byte between: 176 and: 183)
		ifTrue: [pc := nextpc.
			^ byte - 191].
	byte = 237
		ifTrue: [byte := method at: nextpc.
			pc := nextpc + 1.
			^ (extB bitShift: 8)
				+ byte].
	^ nil
