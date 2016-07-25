accessing
simulatedEmphasis
	"Answer the simulatedEmphasis.
	This is 
		0 - normal (no simulatedEmphasis, or simulated regular).
		1 - bold 
		2 - italic
		3 - bold & italic"
	^simulatedEmphasis ifNil:[0]