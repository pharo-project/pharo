private
updateJump: seqId
	"Recalculate final jump bytecodes.  Return true if jump bytecodes SIZE has changed, otherwise return false"
	| pair s1 |
	pair := jumps at: seqId.
	pair ifNil: [^ false].  "no jump, a return"
	bytes := seqBytes  at: seqId.
	s1 := bytes size.
	bytes removeLast: (bytes size - pair first).
	pair last sendTo: self.
	^ s1 ~= bytes size.