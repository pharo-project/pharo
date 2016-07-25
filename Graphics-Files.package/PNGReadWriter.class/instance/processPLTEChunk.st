chunks
processPLTEChunk
	| colorCount i |
	colorCount := chunk size // 3.	"TODO - validate colorCount against depth"
	palette := Array new: colorCount.
	0 
		to: colorCount - 1
		do: 
			[ :index | 
			i := index * 3 + 1.
			palette 
				at: index + 1
				put: (Color 
						r: (chunk at: i) / 255.0
						g: (chunk at: i + 1) / 255.0
						b: (chunk at: i + 2) / 255.0) ]