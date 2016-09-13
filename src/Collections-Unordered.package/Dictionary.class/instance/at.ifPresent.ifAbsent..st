accessing
at: key ifPresent: aPresentBlock ifAbsent: anAbsentBlock
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the value associated
	with the key.
	Otherwise answer the value of the second block."

	self at: key ifPresent: [:v | ^ aPresentBlock cull: v].
	^ anAbsentBlock value