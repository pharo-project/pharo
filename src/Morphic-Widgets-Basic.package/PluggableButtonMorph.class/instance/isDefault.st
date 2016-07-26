accessing
isDefault
	"Answer whether the button is considered to be a default one."

	^self valueOfProperty: #isDefault ifAbsent: [false] 