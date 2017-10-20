utilities
findMaximumLessThan: f in: array

	array size to: 1 by: -1 do: [:i |
		f height >= (array at: i) height ifTrue: [^ array at: i].
	].
	^ array first.
