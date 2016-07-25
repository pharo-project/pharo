class methods
charAtKuten: anInteger

	| a b |
	a := anInteger \\ 100.
	b := anInteger // 100.
	(a > 94) | (b > 94) ifTrue: [
		self error: 'character code is not valid'.
	].
	^ Character leadingChar: self leadingChar code: ((b - 1) * 94) + a - 1.
