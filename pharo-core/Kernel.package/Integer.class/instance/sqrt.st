mathematical functions
sqrt
	"Answer the square root of the receiver."

	| selfAsFloat floatResult guess |
	selfAsFloat := self asFloat.
	floatResult := selfAsFloat sqrt.

	floatResult isInfinite ifFalse: [
		guess := floatResult truncated.

		"If got an exact answer, answer it. Otherwise answer float approximate answer."
		guess squared = self
			ifTrue: [ ^ guess ]].

	"In this case, maybe it failed because we are such a big integer that the Float method becomes
	inexact, even if we are a whole square number. So, try the slower but more general method"
	selfAsFloat >= Float maxExactInteger asFloat squared
		ifTrue: [
			guess := self sqrtFloor.
			guess squared = self ifTrue: [
				^guess ].
			
			"Nothing else can be done. No exact answer means answer must be a Float.
			Answer the best we have which is the rounded sqrt."
			guess := (self * 4) sqrtFloor.
			^(guess // 2 + (guess \\ 2)) asFloat].

	"We need an approximate result"
	^floatResult