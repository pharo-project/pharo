chunks
processInterlaced
	| z filter bytesPerPass startingCol colIncrement rowIncrement startingRow cx sc temp |
	startingCol := #(0 4 0 2 0 1 0 ).
	colIncrement := #(8 8 4 4 2 2 1 ).
	rowIncrement := #(8 8 8 4 4 2 2 ).
	startingRow := #(0 0 4 0 2 0 1 ).
 	z := ZLibReadStream 
 		on: idatChunkStream originalContents
 		from: 1
 		to: idatChunkStream position.
	1 
		to: 7
		do: 
			[ :pass | 
			(self doPass: pass) ifTrue: 
				[ cx := colIncrement at: pass.
				sc := startingCol at: pass.
				bytesPerPass := ((width - sc + cx - 1) // cx * bitsPerPixel + 7) // 8.
				prevScanline := ByteArray new: bytesPerPass.
				thisScanline := ByteArray new: bytesPerScanline.
				(startingRow at: pass) 
					to: height - 1
					by: (rowIncrement at: pass)
					do: 
						[ :y | 
						filter := z next.
						filtersSeen add: filter.
						(filter isNil or: [ (filter 
							between: 0
							and: 4) not ]) ifTrue: [ ^ self ].
						thisScanline := z 
							next: bytesPerPass
							into: thisScanline
							startingAt: 1.
						self 
							filterScanline: filter
							count: bytesPerPass.
						self 
							copyPixels: y
							at: sc
							by: cx.
						temp := prevScanline.
						prevScanline := thisScanline.
						thisScanline := temp ] ] ].
	z atEnd ifFalse: [ self error: 'Unexpected data' ]