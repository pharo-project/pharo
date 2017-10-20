pixel copies
copyPixelsRGB: y
	"Handle non-interlaced RGB color mode (colorType = 2)"

	| i pixel tempForm tempBits |
	(transparentPixelValue isNil and: [ bitsPerChannel = 8 ]) ifTrue: [ "Do the same trick as in #copyPixelsRGBA:"
		| targetIndex |
		tempBits := ByteArray new: thisScanline size * 4 // 3 withAll: 16rFF.
		tempForm := Form extent: width@1 depth: 32 bits: tempBits.
		targetIndex := 1.
		1 to: thisScanline size by: 3 do: [ :index |
			tempBits
				at: targetIndex put: (thisScanline at: index);
				at: targetIndex + 1 put: (thisScanline at: index + 1);
				at: targetIndex + 2 put: (thisScanline at: index + 2).
			targetIndex := targetIndex + 4 ].
		cachedDecoderMap 
			ifNil:[cachedDecoderMap := self rgbaDecoderMapForDepth: depth].
		(BitBlt toForm: form)
			sourceForm: tempForm;
			destOrigin: 0@y;
			combinationRule: Form over;
			colorMap: cachedDecoderMap;
			copyBits.
		^self ].
	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	pixel at: 4 put: 16rFF.
	bitsPerChannel = 8
		ifTrue:
			[i := 1.
			1 to: width do:
				[ :x |
				pixel
					at: 3 put: (thisScanline at: i);
					at: 2 put: (thisScanline at: i+1);
					at: 1 put: (thisScanline at: i+2).
				tempBits at: x put: pixel normalize.
				i := i + 3].
			transparentPixelValue
				ifNotNil:
					[1 to: width do: [ :x |
						(tempBits at: x) = transparentPixelValue
							ifTrue: [tempBits at: x put: 0]]]]
		ifFalse:
			[i := 1.
			1 to: width do:
				[ :x |
				(transparentPixelValue == nil or: [(1 to: 6) anySatisfy: [:k | (transparentPixelValue digitAt: k) ~= (thisScanline at: i + 6 - k)]])
					ifTrue:
						[pixel
							at: 3 put: (thisScanline at: i);
							at: 2 put: (thisScanline at: i+2);
							at: 1 put: (thisScanline at: i+4).
						tempBits at: x put: pixel normalize]
					ifFalse:
						[tempBits at: x put: 0].
				i := i + 6]].
	
	tempForm displayOn: form at: 0@y rule: Form over.
