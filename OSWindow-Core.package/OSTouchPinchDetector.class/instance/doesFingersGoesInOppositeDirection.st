preconditions
doesFingersGoesInOppositeDirection
	| fingersDirection direction |
	fingersDirection := self activeFingers collect: [ :each | each direction ].

	"We suppose that the direction of the gesture is the same as the first finger"
	direction := fingersDirection first.

	"We remove the first so he can't be evaluated a second time"
	fingersDirection at: 1 put: nil.
	^ fingersDirection anySatisfy: [ :finger | finger = direction negated ]