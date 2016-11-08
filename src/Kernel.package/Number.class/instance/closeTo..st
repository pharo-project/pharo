comparing
closeTo: num
	"Tell whether the receiver and arguments are close from each."

	num isFloat ifTrue: [ ^ num closeTo: self asFloat ].
	^ [ self = num ] ifError: [ false ]