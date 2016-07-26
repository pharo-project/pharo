removing
removeFirst: n 
	"Remove first n object into an array"
	| list |
	list := self class arrayType new: n.
	1
		to: n
		do:
			[ : i | list
				at: i
				put: self removeFirst ].
	^ list