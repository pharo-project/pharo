chunks
processNonInterlaced
	| z filter temp copyMethod debug |
	debug := self debugging.
	copyMethod := #(
		#copyPixelsGray:
		nil
		#copyPixelsRGB:
		#copyPixelsIndexed:
		#copyPixelsGrayAlpha:
		nil
		#copyPixelsRGBA:
	) at: colorType + 1.
 	debug ifTrue: [ Transcript cr; nextPutAll: 'NI chunk size='; print: idatChunkStream position ].
 	z := ZLibReadStream 
 		on: idatChunkStream originalContents
 		from: 1
 		to: idatChunkStream position.
	prevScanline := ByteArray new: bytesPerScanline.
	thisScanline := ByteArray new: bytesPerScanline.
	0 
		to: height - 1
		do: 
			[ :y | 
			filter := z next.
			debug ifTrue: [ filtersSeen add: filter ].
			thisScanline := z 
				next: bytesPerScanline
				into: thisScanline
				startingAt: 1.
			(debug and: [ thisScanline size < bytesPerScanline ]) ifTrue: 
				[ Transcript
					nextPutAll: ('wanted {1} but only got {2}' format: {  bytesPerScanline. (thisScanline size)  });
					cr ].
			filter = 0 ifFalse: 
				[ self 
					filterScanline: filter
					count: bytesPerScanline ].
			self 
				perform: copyMethod
				with: y.
			temp := prevScanline.
			prevScanline := thisScanline.
			thisScanline := temp ].
	z atEnd ifFalse: [ self error: 'Unexpected data' ].
	debug ifTrue: 
		[ Transcript
			nextPutAll: ' compressed size=';
			print: z position ]