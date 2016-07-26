find a constant
constMod
	| subTest low |
	"See if mod, (data1 \\ C) is the answer"

	low := answers max.
	low+1 to: low+20 do: [:const |
		subTest := MethodFinder new copy: self addArg: const.
		(subTest testPerfect: #\\) ifTrue: [
			expressions add: 'data1 \\ ', const printString.
			selector add: #\\.
			^ true]].
	^ false