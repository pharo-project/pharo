accessing
leadingCharRunLengthAt: index

	| leadingChar |
	leadingChar := (self at: index) leadingChar.
	index to: self size do: [:i |
		(self at: i) leadingChar ~= leadingChar ifTrue: [^ i - index].
	].
	^ self size - index + 1.
