find a constant
const
	| const |
	"See if (^ constant) is the answer"

	"quick test"
	((const := answers at: 1) closeTo: (answers at: 2)) ifFalse: [^ false].
	3 to: answers size do: [:ii | (const closeTo: (answers at: ii)) ifFalse: [^ false]].
	expressions add: '^ ', const printString.
	selector add: #yourself.
	^ true