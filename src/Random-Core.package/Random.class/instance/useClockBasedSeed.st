initialization
useClockBasedSeed
	"Set a reasonable Park-Miller starting seed [1] based on the ms clock."

	[ seed := (Time millisecondClockValue bitAnd: 16r3FFFFFFF) bitXor: self hash.
	seed = 0 ] whileTrue: [ "Try again if ever get a seed = 0" ]