initialize
initialize
	"
	PNGReadWriter initialize
	"
	BPP := { 
		#(1 2 4 8 16 ).
		#(0 0 0 0 0 ).
		#(0 0 0 24 48 ).
		#(1 2 4 8 0 ).
		#(0 0 0 16 32 ).
		#(0 0 0 0 0 ).
		#(0 0 0 32 64 ).
		#(0 0 0 0 0 )
	 }.
	BlockHeight := #(8 8 4 4 2 2 1 ).
	BlockWidth := #(8 4 4 2 2 1 1 ).
	StandardColors := Color indexedColors collect: 
		[ :aColor | 
		Color 
			r: (aColor red * 255) truncated / 255
			g: (aColor green * 255) truncated / 255
			b: (aColor blue * 255) truncated / 255 ].
	StandardSwizzleMaps := Array new: 4.
	#(1 2 4 ) do: 
		[ :i | 
		StandardSwizzleMaps 
			at: i
			put: (self computeSwizzleMapForDepth: i) ]