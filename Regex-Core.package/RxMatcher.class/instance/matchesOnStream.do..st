match enumeration
matchesOnStream: aStream do: aBlock
	"Be extra careful about successful matches which consume no input.
	After those, make sure to advance or finish if already at end."

	| position subexpression |
	[
		position := aStream position.
		self searchStream: aStream
	] whileTrue: [
		subexpression := self subexpression: 1.
		aBlock value: subexpression.
		subexpression size = 0 ifTrue: [
			aStream atEnd
				ifTrue: [^self]
				ifFalse: [aStream next]]]