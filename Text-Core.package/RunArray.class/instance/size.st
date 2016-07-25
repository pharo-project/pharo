accessing
size
	| size |
	size := 0.
	1 to: runs size do: [:i | size := size + (runs at: i)].
	^size