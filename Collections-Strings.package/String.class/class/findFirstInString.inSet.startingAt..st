primitives
findFirstInString: aString inSet: inclusionMap startingAt: start
	"Trivial, non-primitive version"
	
	| i stringSize ascii more |
	inclusionMap size ~= 256 ifTrue: [^ 0].
	stringSize := aString size.
	more := true.
	i := start - 1.
	[more and: [(i := i + 1) <= stringSize]] whileTrue: [
		ascii := (aString basicAt: i).
		more := ascii < 256 ifTrue: [(inclusionMap at: ascii + 1) = 0] ifFalse: [true].
	].

	i > stringSize ifTrue: [^ 0].
	^ i