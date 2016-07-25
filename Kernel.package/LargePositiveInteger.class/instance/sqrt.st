mathematical functions
sqrt
	"If we know for sure no exact solution exists, then just answer the cheap float approximation without wasting time."
	| selfAsFloat |
	self mightBeASquare
		ifFalse:
			[selfAsFloat := self asFloat.
			selfAsFloat isFinite ifTrue: [^self asFloat sqrt ]].

	"If some exact solution might exist, or self asFloat isInfinite, call potentially expensive super"
	^super sqrt