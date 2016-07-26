smoothing
rgbMap: sourcePixel from: nBitsIn to: nBitsOut 
	"NOTE: This code is copied verbatim from BitBltSimulation so that it
	may be removed from the system"
	"Convert the given pixel value with nBitsIn bits for each color component to a pixel value with nBitsOut bits for each color component. Typical values for nBitsIn/nBitsOut are 3, 5, or 8."
	| mask d srcPix destPix |
	<inline: true>
	(d := nBitsOut - nBitsIn) > 0 
		ifTrue: 
			[ "Expand to more bits by zero-fill"
			mask := (1 << nBitsIn) - 1.	"Transfer mask"
			srcPix := sourcePixel << d.
			mask := mask << d.
			destPix := srcPix bitAnd: mask.
			mask := mask << nBitsOut.
			srcPix := srcPix << d.
			^ destPix + (srcPix bitAnd: mask) + (srcPix << d bitAnd: mask << nBitsOut) ]
		ifFalse: 
			[ "Compress to fewer bits by truncation"
			d = 0 ifTrue: [ ^ sourcePixel ].	"no compression"
			sourcePixel = 0 ifTrue: [ ^ sourcePixel ].	"always map 0 (transparent) to 0"
			d := nBitsIn - nBitsOut.
			mask := (1 << nBitsOut) - 1.	"Transfer mask"
			srcPix := sourcePixel >> d.
			destPix := srcPix bitAnd: mask.
			mask := mask << nBitsOut.
			srcPix := srcPix >> d.
			destPix := destPix + (srcPix bitAnd: mask) + (srcPix >> d bitAnd: mask << nBitsOut).
			destPix = 0 ifTrue: [ ^ 1 ].	"Dont fall into transparent by truncation"
			^ destPix ]