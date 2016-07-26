public access
nextImageDitheredToDepth: depth 
	| form xStep yStep x y bb |
	ditherMask := DitherMasks 
		at: depth
		ifAbsent: [ self error: 'can only dither to display depths' ].
	residuals := WordArray new: 3.
	sosSeen := false.
	self parseFirstMarker.
	[ sosSeen ] whileFalse: [ self parseNextMarker ].
	form := Form 
		extent: width @ height
		depth: depth.
	bb := BitBlt toForm: form.
	bb sourceForm: mcuImageBuffer.
	bb colorMap: (mcuImageBuffer colormapIfNeededFor: form).
	bb sourceRect: mcuImageBuffer boundingBox.
	bb combinationRule: Form over.
	xStep := mcuWidth * DCTSize.
	yStep := mcuHeight * DCTSize.
	y := 0.
	1 
		to: mcuRowsInScan
		do: 
			[ :row | 
			x := 0.
			1 
				to: mcusPerRow
				do: 
					[ :col | 
					self decodeMCU.
					self idctMCU.
					self colorConvertMCU.
					bb
						destX: x;
						destY: y;
						copyBits.
					x := x + xStep ].
			y := y + yStep ].
	^ form