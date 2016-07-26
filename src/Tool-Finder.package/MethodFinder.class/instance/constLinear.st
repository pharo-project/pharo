find a constant
constLinear
	| const subTest got denom num slope offset |
	"See if (data1 * C1) + C2 is the answer.  In the form  #(C2 C1) polynomialEval: data1 "

	denom := ((thisData at: 2) at: 1) - ((thisData at: 1) at: 1).
	denom = 0 ifTrue: [^ false].   "will divide by it"
	num := (answers at: 2) - (answers at: 1).

    slope := (num asFloat / denom) reduce.
    offset := ((answers at: 2) - (((thisData at: 2) at: 1) * slope)) reduce.

	const := Array with: offset with: slope.
	got := (subTest := MethodFinder new copy: self addArg: const) 
				searchForOne isEmpty not.
	got ifFalse: [^ false]. 

	"replace data2 with const in expressions"
	subTest expressions do: [:exp |
		expressions add: (exp copyReplaceAll: 'data2' with: const printString)].
	selector addAll: subTest selectors.
	^ true