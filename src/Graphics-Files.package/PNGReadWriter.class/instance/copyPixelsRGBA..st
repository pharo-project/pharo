pixel copies
copyPixelsRGBA: y
	"Handle non-interlaced RGBA color modes (colorType = 6)"

	| i pixel tempForm tempBits ff |
	bitsPerChannel = 8 ifTrue: [
		ff := Form extent: width@1 depth: 32 bits: thisScanline.
		cachedDecoderMap 
			ifNil:[cachedDecoderMap := self rgbaDecoderMapForDepth: depth].
		(BitBlt toForm: form)
			sourceForm: ff;
			destOrigin: 0@y;
			combinationRule: Form over;
			colorMap: cachedDecoderMap;
			copyBits.
		^self.
	].
	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	i := -7.
	0 to: width-1 do: [ :x |
			i := i + 8.
			pixel at: 4 put: (thisScanline at: i+6);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+2);
				at: 1 put: (thisScanline at: i+4).
			tempBits at: x+1 put: pixel normalize.
	].
	tempForm displayOn: form at: 0@y rule: Form over.
