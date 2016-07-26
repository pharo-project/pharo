parsing-private
peekSignIsMinus
	"Peek an optional sign from sourceStream.
	Answer true if it is minus sign"

	| isMinus |
	isMinus := sourceStream peekFor: $-.
	isMinus ifFalse: [self allowPlusSign ifTrue: [sourceStream peekFor: $+]].
	^isMinus