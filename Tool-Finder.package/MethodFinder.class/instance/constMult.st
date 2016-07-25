find a constant
constMult
	| const subTest got |
	"See if (data1 * C) is the answer"

	((thisData at: 1) at: 1) = 0 ifTrue: [^ false].
	const := ((answers at: 1) asFloat / ((thisData at: 1) at: 1)) reduce.
	got := (subTest := MethodFinder new copy: self addArg: const) 
				searchForOne isEmpty not.
	got ifFalse: [^ false]. 

	"replace data2 with const in expressions"
	subTest expressions do: [:exp |
		expressions add: (exp copyReplaceAll: 'data2' with: const printString)].
	selector addAll: subTest selectors.
	^ true