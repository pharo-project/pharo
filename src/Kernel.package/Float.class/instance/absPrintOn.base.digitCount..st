private
absPrintOn: aStream base: base digitCount: digitCount 
	"Print me in the given base, using digitCount significant figures."

	| fuzz x exp q fBase scale logScale xi |
	self isInfinite ifTrue: [^ aStream nextPutAll: 'Infinity'].
	fBase := base asFloat.
	"x is myself normalized to [1.0, fBase), exp is my exponent"
	exp := self floorLog: fBase.
	scale := 1.0.
	logScale := 0.
	[(x := fBase raisedTo: (exp + logScale)) = 0]
		whileTrue:
			[scale := scale * fBase.
			logScale := logScale + 1].
	x := self * scale / x.
	fuzz := fBase raisedTo: 1 - digitCount.
	"round the last digit to be printed"
	x := 0.5 * fuzz + x.
	x >= fBase
		ifTrue: 
			["check if rounding has unnormalized x"
			x := x / fBase.
			exp := exp + 1].
	(exp < 6 and: [exp > -4])
		ifTrue: 
			["decimal notation"
			q := 0.
			exp < 0 ifTrue: [1 to: 1 - exp do: [:i | aStream nextPut: ('0.0000'
at: i)]]]
		ifFalse: 
			["scientific notation"
			q := exp.
			exp := 0].
	[x >= fuzz]
		whileTrue: 
			["use fuzz to track significance"
			xi := x asInteger.
			aStream nextPut: (Character digitValue: xi).
			x := x - xi asFloat * fBase.
			fuzz := fuzz * fBase.
			exp := exp - 1.
			exp = -1 ifTrue: [aStream nextPut: $.]].
	[exp >= -1]
		whileTrue: 
			[aStream nextPut: $0.
			exp := exp - 1.
			exp = -1 ifTrue: [aStream nextPut: $.]].
	q ~= 0
		ifTrue: 
			[aStream nextPut: $e.
			q printOn: aStream]