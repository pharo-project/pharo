tests - characterization
testMaxExactInteger
	"
	FloatTest new testMaxExactInteger
	"

	self assert: Float maxExactInteger asFloat truncated = Float maxExactInteger.
	0 to: 10000 do: [ :j |
		self assert: (Float maxExactInteger-j) asFloat truncated = (Float maxExactInteger-j) ].
	self deny: (Float maxExactInteger+1) asFloat truncated = (Float maxExactInteger+1)
	