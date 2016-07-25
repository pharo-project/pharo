accessing
amountLeft
	"Answer the amount that has not yet been transferred.
	Can be nil. Should be between 0 and total."

	^ self isEmpty ifFalse: [ total - amount ]