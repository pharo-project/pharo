controlling
return: value
	"Unwind thisContext to self and return value to self's sender.  Execute any unwind blocks while unwinding.  ASSUMES self is a sender of thisContext"

	sender ifNil: [^self cannotReturn: value to: sender].
	sender resume: value