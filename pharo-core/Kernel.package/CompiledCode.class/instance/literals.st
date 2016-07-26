literals
literals
	"Answer an Array of the literals referenced by the receiver."
	| literals numberLiterals |
	literals := Array new: (numberLiterals := self numLiterals).
	1 to: numberLiterals do: [:index |
		literals at: index put: (self objectAt: index + 1)].
	^literals