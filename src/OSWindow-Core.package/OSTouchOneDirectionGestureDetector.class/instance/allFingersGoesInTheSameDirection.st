preconditions
allFingersGoesInTheSameDirection
	"We suppose that the direction of the gesture is the same as the first finger"
	direction := self activeFingers first direction.
	
	^ activeFingers allSatisfy: [ :finger | finger direction = direction ]