stream in
peekForAll: aString
	"Answer whether or not the next string of characters in the receiver
	matches aString. If a match is made, advance over that string in the receiver and
	answer true. If no match, then leave the receiver alone and answer false.
	We use findString:startingAt: to avoid copying.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!"

	| sz start |
	sz := aString size.
	self receiveData: sz.
	(inNextToWrite - lastRead - 1) < sz ifTrue: [^false].
	start := lastRead + 1.
	(inBuffer findString: aString startingAt: start) = start
		ifFalse: [^false].
	lastRead := lastRead + sz.
	^true