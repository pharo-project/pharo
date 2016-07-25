private
unpackBits: bits depthTo8From: depth with: width height: height pad: pad 
	"Unpack bits of depth 1, 2, or 4 image to it of depth 8 image."
	| bitMask pixelInByte bitsWidth upBitsWidth stopWidth trailingSize upBits bitIndex upBitIndex val |
	(#(1 2 4 ) includes: depth) ifFalse: [ ^ self error: 'depth must be 1, 2, or 4' ].
	(#(8 16 32 ) includes: pad) ifFalse: [ ^ self error: 'pad must be 8, 16, or 32' ].
	bitMask := (1 bitShift: depth) - 1.
	pixelInByte := 8 / depth.
	bitsWidth := (width * depth + pad - 1) // pad * (pad / 8).
	upBitsWidth := (width * 8 + pad - 1) // pad * (pad / 8).
	stopWidth := (width * depth + 7) // 8.
	trailingSize := width - ((stopWidth - 1) * pixelInByte).
	upBits := ByteArray new: upBitsWidth * height.
	1 
		to: height
		do: 
			[ :i | 
			bitIndex := (i - 1) * bitsWidth.
			upBitIndex := (i - 1) * upBitsWidth.
			1 
				to: stopWidth - 1
				do: 
					[ :j | 
					val := bits at: (bitIndex := bitIndex + 1).
					upBitIndex := upBitIndex + pixelInByte.
					1 
						to: pixelInByte
						do: 
							[ :k | 
							upBits 
								at: upBitIndex - k + 1
								put: (val bitAnd: bitMask).
							val := val bitShift: depth negated ] ].
			val := (bits at: (bitIndex := bitIndex + 1)) bitShift: depth negated * (pixelInByte - trailingSize).
			upBitIndex := upBitIndex + trailingSize.
			1 
				to: trailingSize
				do: 
					[ :k | 
					upBits 
						at: upBitIndex - k + 1
						put: (val bitAnd: bitMask).
					val := val bitShift: depth negated ] ].
	^ upBits