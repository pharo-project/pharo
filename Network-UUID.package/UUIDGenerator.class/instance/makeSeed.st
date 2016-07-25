random seed
makeSeed
	"Try various methods of getting good seeds"
	| seed |
	seed := self makeUnixSeed.
	seed ifNotNil: [^seed].

	"not sure if this is reliably random... commented out for now. -dew"
	"seed := self makeSeedFromSound.
	seed ifNotNil: [^seed]."
	
	"default"
	[seed := (Time millisecondClockValue bitAnd: 16r3FFFFFFF) bitXor: self hash.
	seed := seed bitXor: (Time totalSeconds bitAnd: 16r3FFFFFFF).
	seed = 0] whileTrue: ["Try again if ever get a seed = 0"].

	^seed
