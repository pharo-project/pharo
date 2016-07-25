private-functions
iiA: a B: b C: c D: d M: m S: s T: t
	"compute a = b + ((a + i(b,c,d) + m + t) <<< s)"
	^ a += (self iX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.
