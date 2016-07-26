stepping and presenter
stepTime 
	"Adjust my step time to the time it takes drawing my referent"
	drawTime ifNil:[^ 250].
	^(20 * drawTime) max: 250.