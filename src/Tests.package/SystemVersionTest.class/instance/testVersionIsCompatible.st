tests - compatibility
testVersionIsCompatible
	"Test that the result of the #version method follows the historic pattern (e.g. Pharo7.0alpha)
	 and is not broken"
	 
	self checkIsCompatibleWithResultOfVersionMessage: (SystemVersion major: 7 minor: 0 suffix: 'alpha' build: 148) version