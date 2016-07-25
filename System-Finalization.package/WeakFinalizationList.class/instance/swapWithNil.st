accessing
swapWithNil

	| head |
	head := first.
	first := nil.
	^ head