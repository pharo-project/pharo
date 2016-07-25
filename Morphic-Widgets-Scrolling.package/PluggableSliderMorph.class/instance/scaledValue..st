accessing
scaledValue: newValue
	"Set the scaled value."

	|val|
	val := newValue.
	self quantum ifNotNil: [:q |
		val := val roundTo: q].
	self value: (self max <= self min
		ifTrue: [0]
		ifFalse: [val - self min / (self max - self min)])