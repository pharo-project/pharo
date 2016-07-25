multibyte character methods
fixXTable
	| newXTable val |
	xTable size >= 258 ifTrue: [ ^ self ].
	newXTable := Array new: 258.
	1 
		to: xTable size
		do: 
			[ :i | 
			newXTable 
				at: i
				put: (xTable at: i) ].
	val := xTable at: xTable size.
	xTable size + 1 
		to: 258
		do: 
			[ :i | 
			newXTable 
				at: i
				put: val ].
	minAscii := 0.
	maxAscii := 255.
	xTable := newXTable