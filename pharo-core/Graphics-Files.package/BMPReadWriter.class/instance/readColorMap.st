reading
readColorMap
	"Read colorCount BMP color map entries from the given binary stream. Answer an array of Colors."
	| colorCount colors maxLevel b g r ccStream |
	colorCount := (bfOffBits - 54) // 4.
	"Note: some programs (e.g. Photoshop 4.0) apparently do not set colorCount; assume that any data between the end of the header and the start of the pixel data is the color map"
	biBitCount >= 16 ifTrue: [ ^ nil ].
	colorCount = 0 ifTrue: 
		[ "this BMP file does not have a color map"
		"default monochrome color map"
		biBitCount = 1 ifTrue: 
			[ ^ Array 
				with: Color white
				with: Color black ].
		"default gray-scale color map"
		maxLevel := (2 raisedTo: biBitCount) - 1.
		^ (0 to: maxLevel) collect: [ :level | Color gray: level asFloat / maxLevel ] ].
	ccStream := (stream next: colorCount * 4) readStream.
	colors := Array new: colorCount.
	1 
		to: colorCount
		do: 
			[ :i | 
			b := ccStream next.
			g := ccStream next.
			r := ccStream next.
			ccStream next.	"skip reserved"
			colors 
				at: i
				put: (Color 
						r: r
						g: g
						b: b
						range: 255) ].
	^ colors