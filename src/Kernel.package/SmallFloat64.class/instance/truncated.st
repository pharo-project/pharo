truncation and round off
truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 551>
	(self isInfinite or: [self isNaN]) ifTrue: [self error: 'Cannot truncate this number'].

	self abs < 2.0e16
		ifTrue: ["Fastest way when it may not be an integer"
				"^ (self quo: 1073741823.0) * 1073741823 + (self rem: 1073741823.0) truncated"
				| di df q r |
				di := (SmallInteger maxVal bitShift: -1)+1.
				df := di asFloat.
				q := self quo: df.
				r := self - (q asFloat * df).
				^q*di+r truncated]
		ifFalse: [^ self asTrueFraction.  "Extract all bits of the mantissa and shift if necess"]

		

		