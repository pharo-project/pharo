public
computeSignatureForMessageHash: hash privateKey: privateKey
	"Answer the digital signature of the given message hash using the given private key. A signature is a pair of large integers. The private key is an array of four large integers: (p, q, g, x)."

	| p q g x r s k tmp |
	p := privateKey first.
	q := privateKey second.
	g := privateKey third.
	x := privateKey fourth.

	r := s := 0.
	[r = 0 or: [s = 0]] whileTrue: [
		k := self nextRandom160 \\ q.
		r := (g raisedTo: k modulo: p) \\ q.
		tmp := (hash + (x * r)) \\ q.
		s := ((k reciprocalModulo: q) * tmp) \\ q].

	^ Array with: r with: s
