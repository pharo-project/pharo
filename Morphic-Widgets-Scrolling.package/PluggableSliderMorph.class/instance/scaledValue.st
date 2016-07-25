accessing
scaledValue
	"Answer the scaled value."

	|val|
	val := self value * (self max - self min) + self min.
	self quantum ifNotNil: [:q |
		val := val roundTo: q].
	^(val max: self min) min: self max