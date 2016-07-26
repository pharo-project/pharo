accessing
findLastOccurrenceOfString: substring startingAt: start 
	"Answer the index of the last occurrence of substring within the receiver, starting at start. If 
	the receiver does not contain substring, answer 0.  Case-sensitive match used."

	| last now |
	last := self findString: substring startingAt: start.
	last = 0 ifTrue: [^ 0].
	[last > 0] whileTrue:
		[now := last.
		last := self findString: substring startingAt: last + 1].

	^ now
