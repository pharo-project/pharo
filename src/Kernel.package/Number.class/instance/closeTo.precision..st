comparing
closeTo: num precision: aPrecision
	"are these two numbers close?"

	num isFloat ifTrue: [ ^ num closeTo: self asFloat precision: aPrecision ].
	^ [ self = num ] ifError: [ false ]