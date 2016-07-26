testing
isFraction
	"Each Integer is considered as a special kind of Fraction with self as numerator and a unit denominator.
	Rationale: A Fraction with a unit denominator will be automatically reduced to an Integer.
	Hence Integer has to be polymorphic to Fraction."
	^true