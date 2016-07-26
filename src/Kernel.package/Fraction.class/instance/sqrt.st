mathematical functions
sqrt
	| d n |
	n := numerator sqrt.
	d := denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	^n / d