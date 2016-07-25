public
verifySignature: aSignature ofMessageHash: hash publicKey: publicKey
	"Answer true if the given signature is the authentic signature of the given message hash. That is, if the signature must have been computed using the private key set corresponding to the given public key. The public key is an array of four large integers: (p, q, g, y)."

	| p q g y r s w u1 u2 v0 v |
	p := publicKey first.
	q := publicKey second.
	g := publicKey third.
	y := publicKey fourth.
	r := aSignature first.
	s := aSignature last.
	((r > 0) and: [r < q]) ifFalse: [^ false].  "reject"
	((s > 0) and: [s < q]) ifFalse: [^ false].  "reject"

	w := s reciprocalModulo: q.
	u1 := (hash * w) \\ q.
	u2 := (r * w) \\ q.
	v0 := (g raisedTo: u1 modulo: p) * (y raisedTo: u2 modulo: p).
	v := ( v0 \\ p) \\ q.
	^ v = r
