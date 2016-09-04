initialization
on: aCollection from: firstIndex to: lastIndex
	bitBuf := bitPos := 0.
	"The decompression buffer has a size of at 64k,
	since we may have distances up to 32k back and
	repetitions of at most 32k length forward"
	collection := aCollection species new: 1 << 16.
	readLimit := 0. "Not yet initialized"
	position := 0.
	source := aCollection.
	sourceLimit := lastIndex.
	sourcePos := firstIndex-1.
	state := StateNewBlock.