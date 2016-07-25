accessing
bounds
	| min max |
	min := max := self at: 1.
	self do:[:pt|
		min := min min: pt.
		max := max max: pt].
	^min corner: max
		