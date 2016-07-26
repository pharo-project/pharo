accessing
amountLeft: byteCount
	"Set the amount that has not yet been transferred.
	Can be nil. Should be between 0 and total."

	^ total ifNotNil: [ amount := total - byteCount ]