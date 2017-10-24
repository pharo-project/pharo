accessing
size
	| size |
	size := 0.
	self do: [ :_ | size := size + 1].
	^ size