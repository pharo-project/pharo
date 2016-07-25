marker parsing
initialSOSSetup
	mcuWidth := (components detectMax: [ :c | c widthInBlocks ]) widthInBlocks.
	mcuHeight := (components detectMax: [ :c | c heightInBlocks ]) heightInBlocks.
	components do: 
		[ :c | 
		c 
			mcuWidth: mcuWidth
			mcuHeight: mcuHeight
			dctSize: DCTSize ].
	stream resetBitBuffer