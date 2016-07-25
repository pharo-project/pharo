comparing
closeTo: num
	"are these two numbers close?"

	num isFloat ifTrue: [^ num closeTo: self asFloat].
	^[self = num] ifError: [false]