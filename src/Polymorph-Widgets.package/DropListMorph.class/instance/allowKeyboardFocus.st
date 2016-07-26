accessing
allowKeyboardFocus
	"Answer whether or not keyboard focus will be allowed."

	^self valueOfProperty: #allowKeyboardFocus ifAbsent: [true]