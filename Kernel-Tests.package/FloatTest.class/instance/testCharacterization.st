tests - characterization
testCharacterization

	"Test the largest finite representable floating point value"
	self assert: Float fmax successor = Float infinity.
	self assert: Float infinity predecessor = Float fmax.
	self assert: Float fmax negated predecessor = Float infinity negated.
	self assert: Float infinity negated successor = Float fmax negated.
	
	"Test the smallest positive representable floating point value"
	self assert: Float fmin predecessor = 0.0.
	self assert: 0.0 successor = Float fmin.
	self assert: Float fmin negated successor = 0.0.
	self assert: 0.0 predecessor = Float fmin negated.
	
	"Test the relative precision"
	self assert: Float one + Float epsilon > Float one.
	self assert: Float one + Float epsilon = Float one successor.
	self assert: Float one + (Float epsilon / Float radix) = Float one.
	
	"Test maximum and minimum exponent"
	self assert: Float fmax exponent = Float emax.
	self assert: Float fminNormalized exponent = Float emin.
	Float denormalized ifTrue: [
		self assert: Float fminDenormalized exponent = (Float emin + 1 - Float precision)].
	
	"Alternative tests for maximum and minimum"
	self assert: (Float radix - Float epsilon) * (Float radix raisedTo: Float emax) = Float fmax.
	self assert: Float epsilon * (Float radix raisedTo: Float emin) = Float fmin.
	
	"Test sucessors and predecessors"
	self assert: Float one predecessor successor = Float one.
	self assert: Float one successor predecessor = Float one.
	self assert: Float one negated predecessor successor = Float one negated.
	self assert: Float one negated successor predecessor = Float one negated.
	self assert: Float infinity successor = Float infinity.
	self assert: Float infinity negated predecessor = Float infinity negated.
	self assert: Float nan predecessor isNaN.
	self assert: Float nan successor isNaN.
	
	"SPECIFIC FOR IEEE 754 double precision - 64 bits"
	self assert: Float fmax hex = '7FEFFFFFFFFFFFFF'.
	self assert: Float fminDenormalized hex = '0000000000000001'.
	self assert: Float fminNormalized hex = '0010000000000000'.
	self assert: 0.0 hex = '0000000000000000'.
	self assert: Float negativeZero hex = '8000000000000000'.
	self assert: Float one hex = '3FF0000000000000'.
	self assert: Float infinity hex = '7FF0000000000000'.
	self assert: Float infinity negated hex = 'FFF0000000000000'.