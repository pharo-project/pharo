testing
isSelfPseudoVariable
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'self' or: [name = '{{self}}']