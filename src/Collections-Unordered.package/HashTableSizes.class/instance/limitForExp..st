private
limitForExp: exp
	"Answer the largest integer that isn't too close to the next higher power of 2 than exp."
	
	| expLimit |
	expLimit := exp ceiling - (0.5 / self numValuesPerPower).
	^(2 raisedTo: expLimit) rounded.
	