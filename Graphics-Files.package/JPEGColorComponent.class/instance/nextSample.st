sample streaming
nextSample
	| dx dy blockIndex sampleIndex sample |
	dx := currentX // hSampleFactor.
	dy := currentY // vSampleFactor.
	blockIndex := dy // dctSize * widthInBlocks + (dx // dctSize) + 1.
	sampleIndex := dy \\ dctSize * dctSize + (dx \\ dctSize) + 1.
	sample := (mcuBlocks at: blockIndex) at: sampleIndex.
	currentX := currentX + 1.
	currentX < (mcuWidth * dctSize) ifFalse: 
		[ currentX := 0.
		currentY := currentY + 1 ].
	^ sample