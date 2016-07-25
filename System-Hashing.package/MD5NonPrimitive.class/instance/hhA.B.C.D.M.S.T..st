private-functions
hhA: a B: b C: c D: d M: m S: s T: t
	"compute a = b + ((a + h(b,c,d) + m + t) <<< s)"
	^ a += (self hX: b Y: c Z: d); += m; += t; leftRotateBy: s; += b.
