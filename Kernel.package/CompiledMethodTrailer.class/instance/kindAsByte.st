private
kindAsByte
	| index |
	index := self class trailerKinds indexOf: kind.
	[index ~~ 0] assert.

	^ (index - 1) << 2