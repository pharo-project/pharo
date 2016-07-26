removing
removeLast: n 
	"Remove last n object into an array with last in last position"
	| list |
	list := self class arrayType new: n.
	n
		to: 1
		by: -1
		do:
			[ : i | list
				at: i
				put: self removeLast ].
	^ list