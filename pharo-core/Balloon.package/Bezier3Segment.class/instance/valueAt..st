accessing
valueAt: t
	| a b c d |

	"| p1 p2 p3 |
	p1 := start interpolateTo: via1 at: t.
	p2 := via1 interpolateTo: via2 at: t.
	p3 := via2 interpolateTo: end at: t.
	p1 := p1 interpolateTo: p2 at: t.
	p2 := p2 interpolateTo: p3 at: t.
	^ p1 interpolateTo: p2 at: t"

	a := (start negated) + (3 * via1) - (3 * via2) + (end).
	b := (3 * start) - (6 * via1) + (3 * via2).
	c := (3 * start negated) + (3 * via1).
	d := start.
	^ ((a * t + b) * t + c) * t + d

