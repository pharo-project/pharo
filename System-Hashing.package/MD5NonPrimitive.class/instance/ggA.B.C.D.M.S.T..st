private-functions
ggA: a B: b C: c D: d M: m S: s T: t
	"compute a = b + ((a + g(b,c,d) + m + t) <<< s)"
	^ a += (self gX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.
