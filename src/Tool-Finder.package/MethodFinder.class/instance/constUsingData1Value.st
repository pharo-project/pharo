find a constant
constUsingData1Value
	| subTest |
	"See if (data1 <= C) or (data1 >= C) is the answer"

	"quick test"
	((answers at: 1) class superclass == Boolean) ifFalse: [^ false].
	2 to: answers size do: [:ii | 
		((answers at: ii) class superclass == Boolean) ifFalse: [^ false]].

	thisData do: [:datums | | const got | 
		const := datums first.	"use data as a constant!"
		got := (subTest := MethodFinder new copy: self addArg: const) 
					searchForOne isEmpty not.
		got ifTrue: [
			"replace data2 with const in expressions"
			subTest expressions do: [:exp |
				expressions add: (exp copyReplaceAll: 'data2' with: const printString)].
			selector addAll: subTest selectors.
			^ true]].
	^ false