private-decoding
readColorTable: numberOfEntries 
	| array r g b |
	array := Array new: numberOfEntries.
	1 
		to: array size
		do: 
			[ :i | 
			r := self next.
			g := self next.
			b := self next.
			array 
				at: i
				put: (Color 
						r: r
						g: g
						b: b
						range: 255) ].
	^ array