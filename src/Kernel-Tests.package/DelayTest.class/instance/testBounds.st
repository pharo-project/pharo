tests
testBounds
	"self run: #testBounds"

	self should: [ Delay forMilliseconds: -1 ] raise: Error.
	Delay forMilliseconds: SmallInteger maxVal // 2 + 1.
	Delay forMilliseconds: SmallInteger maxVal + 1.
	(Delay forMilliseconds: Float pi) wait	"Wait 3ms"