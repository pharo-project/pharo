printing
absPrintInexactlyOn: aStream base: base
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion."

	| significantBits fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
	self isInfinite ifTrue: [aStream nextPutAll: 'Float infinity'. ^ self].
	significantBits := 50.  "approximately 3 lsb's of accuracy loss during conversion"
	fBase := base asFloat.
	exp := self exponent.
	baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[r := self.
			s := 1.0.
			mPlus := 1.0 timesTwoPower: exp - significantBits.
			mMinus := self significand ~= 1.0 ifTrue: [mPlus] ifFalse: [mPlus / 2.0]]
		ifFalse:
			[r := self timesTwoPower: significantBits.
			s := 1.0 timesTwoPower:  significantBits.
			mMinus := 1.0 timesTwoPower: (exp max: -1024).
			mPlus :=
				(exp = MinValLogBase2) | (self significand ~= 1.0)
					ifTrue: [mMinus]
					ifFalse: [mMinus * 2.0]].
	baseExpEstimate >= 0
		ifTrue:
			[s := s * (fBase raisedToInteger: baseExpEstimate).
			exp = 1023
				ifTrue:   "scale down to prevent overflow to Infinity during conversion"
					[r := r / fBase.
					s := s / fBase.
					mPlus := mPlus / fBase.
					mMinus := mMinus / fBase]]
		ifFalse:
			[exp < -1023
				ifTrue:   "scale up to prevent denorm reciprocals overflowing to Infinity"
					[d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
					scale := fBase raisedToInteger: d.
					r := r * scale.
					mPlus := mPlus * scale.
					mMinus := mMinus * scale.
					scale := fBase raisedToInteger: (baseExpEstimate + d) negated]
				ifFalse:
				[scale := fBase raisedToInteger: baseExpEstimate negated].
			s := s / scale].
	(r + mPlus >= s)
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[s := s / fBase].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	[d := (r / s) truncated.
	r := r - (d * s).
	(tc1 := r <= mMinus) | (tc2 := r + mPlus >= s)] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * fBase.
		mPlus := mPlus * fBase.
		mMinus := mMinus * fBase.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2.0 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]