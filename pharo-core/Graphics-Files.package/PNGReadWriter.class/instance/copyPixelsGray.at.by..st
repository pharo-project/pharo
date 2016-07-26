pixel copies
copyPixelsGray: y at: startX by: incX
	"Handle interlaced grayscale color mode (colorType = 0)"

	| offset bits blitter pixPerByte shifts b pixel mask pixelNumber |
	bitsPerChannel = 16
		ifTrue: [
			"Warning: This is extremely slow. Besides we are downsampling to 8 bits!"
			blitter := BitBlt bitPokerToForm: form.
			startX to: width-1 by: incX do: [ :x |
				| high low value |
				high := thisScanline at: x//incX<<1 + 1.
				low := thisScanline at: x//incX<<1 + 2.
				value := (high * 256 + low = transparentPixelValue)
					ifTrue: [0 "transparent"]
					ifFalse: [high max: 1].
				blitter pixelAt: x @ y put: value ].
				^self ].
	offset := y*rowSize+1.
	bits := form bits.
	bitsPerChannel = 8 ifTrue: [
		startX to: width-1 by: incX do: [ :x | | w |
			w := offset + (x>>2).
			b := 3- (x \\ 4) * 8.
			pixel := (thisScanline at: x // incX + 1)<<b.
			mask := (255<<b) bitInvert32.
			bits at: w put: (((bits at: w) bitAnd: mask) bitOr: pixel)
		].
		^ self
	].
	bitsPerChannel = 1 ifTrue: [
		pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0).
	].
	bitsPerChannel = 2 ifTrue: [
		pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0).
	].
	bitsPerChannel = 4 ifTrue: [
		pixPerByte := 2.
		mask := 15.
		shifts := #(4 0).
	].

	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX to: width-1 by: incX do: [ :x | | rawByte |
		rawByte := thisScanline at: (pixelNumber // pixPerByte) + 1.
		pixel := (rawByte >> (shifts at: (pixelNumber \\ pixPerByte) + 1)) bitAnd: mask.
		blitter pixelAt: (x@y) put: pixel.
		pixelNumber := pixelNumber + 1.
	].
