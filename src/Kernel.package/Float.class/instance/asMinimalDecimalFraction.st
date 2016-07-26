printing
asMinimalDecimalFraction
	"Answer the shortest decimal Fraction that will equal self when converted back asFloat.
	A decimal Fraction has only powers of 2 and 5 as decnominator.
	For example, 0.1 asMinimalDecimalFraction = (1/10)."

	| significand exp baseExpEstimate r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount slowbit shead denominator numerator |
	self isFinite ifFalse: [self error: 'Only finite Float can be converted to a Fraction'].
	self = 0.0 ifTrue: [^0].
	self < 0.0 ifTrue: [^self negated asMinimalDecimalFraction negated].
	numerator := 0.
	denominator := 0.
	significand := self significandAsInteger.
	roundingIncludesLimits := significand even.
	exp := (self exponent - 52) max: MinValLogBase2.
	baseExpEstimate := (self exponent * 10 asFloat reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[significand ~= 16r10000000000000
				ifTrue:
					[r := significand bitShift: 1 + exp.
					s := 2.
					mPlus := mMinus := 1 bitShift: exp]
				ifFalse:
					[r := significand bitShift: 2 + exp.
					s := 4.
					mPlus := 2 * (mMinus := 1 bitShift: exp)]]
		ifFalse:
			[(exp = MinValLogBase2 or: [significand ~= 16r10000000000000])
				ifTrue:
					[r := significand bitShift: 1.
					s := 1 bitShift: 1 - exp.
					mPlus := mMinus := 1]
				ifFalse:
					[r := significand bitShift: 2.
					s := 1 bitShift: 2 - exp.
					mPlus := 2.
					mMinus := 1]].
	baseExpEstimate >= 0
		ifTrue: [s := s * (10 raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale := 10 raisedToInteger: baseExpEstimate negated.
			r := r * scale.
			mPlus := mPlus * scale.
			mMinus := mMinus * scale].
	((r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[r := r * 10.
			mPlus := mPlus * 10.
			mMinus := mMinus * 10].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [denominator := 10 raisedTo: baseExpEstimate negated]]
		ifFalse:
			[decPointCount := 1].
	slowbit := 1 - s lowBit .
	shead := s bitShift: slowbit.
	[d := (r bitShift: slowbit) // shead.
	r := r - (d * s).
	(tc1 := (r > mMinus) not and: [roundingIncludesLimits or: [r < mMinus]]) |
	(tc2 := (r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])] whileFalse:
		[numerator := 10 * numerator + d.
		denominator := 10 * denominator.
		r := r * 10.
		mPlus := mPlus * 10.
		mMinus := mMinus * 10.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [denominator := 1]].
	tc2 ifTrue:
		[(tc1 not or: [r * 2 >= s]) ifTrue: [d := d + 1]].
	numerator := 10 * numerator + d.
	denominator := 10 * denominator.
	decPointCount > 0
		ifTrue:
		[numerator := (10 raisedTo: decPointCount - 1) * numerator].
	fixedFormat ifFalse:
		[(baseExpEstimate - 1) > 0
			ifTrue: [numerator := (10 raisedTo: baseExpEstimate - 1) * numerator]
			ifFalse: [denominator := (10 raisedTo: 1 - baseExpEstimate) * (denominator max: 1)]].
	denominator < 2 ifTrue: [^numerator].
	^numerator / denominator