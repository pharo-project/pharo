accessing
args
	| args |
	args := Array new: numArgs.
	1 to: numArgs do: [ :idx | args at: idx put: self -> (idx - 1)].
	^ args