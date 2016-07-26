constants
epsilon
	"Answer difference between 1.0 and previous representable value"
	
	^1.0 timesTwoPower: 1 - self precision