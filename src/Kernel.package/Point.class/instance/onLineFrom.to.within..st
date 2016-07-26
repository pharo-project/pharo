point functions
onLineFrom: p1 to: p2 within: epsilon
	"Answer true if the receiver lies on the given line segment between p1 and p2 within a small epsilon."

	"is this point within the box spanning p1 and p2 expanded by epsilon? (optimized)"
	p1 x < p2 x
		ifTrue: [
			((x < (p1 x - epsilon)) or: [x > (p2 x + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((x < (p2 x - epsilon)) or: [x > (p1 x + epsilon)]) ifTrue: [^ false]].
	p1 y < p2 y
		ifTrue: [
			((y < (p1 y - epsilon)) or: [y > (p2 y + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((y < (p2 y - epsilon)) or: [y > (p1 y + epsilon)]) ifTrue: [^ false]].

	"it's in the box; is it on the line?"
	^ (self dist: (self nearestPointAlongLineFrom: p1 to: p2)) <= epsilon