private-functions
ffA: a B: b C: c D: d M: m S: s T: t
	"compute a = b + ((a + f(b,c,d) + m + t) <<< s)"
	^ a += (self fX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.
