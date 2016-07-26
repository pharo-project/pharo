testing
isConflict
	"Answer whether the receiver is a conflict item."

	| op |
	op := self operation.
	^ op notNil and: [op isConflict ]