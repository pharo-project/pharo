private
popTo: oldPos

	| t |
	t := Array new: statements size - oldPos.
	(t size to: 1 by: -1) do:
		[:i | t at: i put: statements removeLast].
	^t