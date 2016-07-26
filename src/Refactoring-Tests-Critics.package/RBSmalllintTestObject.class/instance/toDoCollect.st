methods
toDoCollect
	| array |
	array := Array new: 10.
	1 to: 10 do: [:i | array at: i put: i * i].
	^array