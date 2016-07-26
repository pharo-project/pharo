collection ops
size
	| size |
	size := 0.
	map
		keysAndValuesDo: [:high :lowmap | self
				bitmap: lowmap
				do: [:low | size := size + 1]].
	^ size