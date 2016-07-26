mathematical functions
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."

	| d n |
	n := numerator nthRoot: aPositiveInteger.
	d := denominator nthRoot: aPositiveInteger.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat nthRoot: aPositiveInteger ].
	^n / d