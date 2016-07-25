pixel copies
copyPixelsIndexed: y
	"Handle non-interlaced indexed color mode (colorType = 3)"
	| hack hackBlt swizzleHack swizzleBlt scanline hackDepth |
	scanline := ByteArray new: bytesPerScanline + 3 // 4 * 4.
	scanline replaceFrom: 1 to: thisScanline size with: thisScanline startingAt: 1.
	hackDepth := bigEndian ifTrue:[form depth] ifFalse:[form depth negated].
	hack := Form extent: width@1 depth: hackDepth bits: scanline.
	hackBlt := BitBlt toForm: form.
	hackBlt sourceForm: hack.
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@y.
	hackBlt width: width; height: 1.

	(form depth < 8 and:[bigEndian not]) ifTrue:[
		swizzleHack := Form new hackBits: scanline.
		swizzleBlt := BitBlt toForm: swizzleHack.
		swizzleBlt sourceForm: swizzleHack.
		swizzleBlt combinationRule: Form over.
		swizzleBlt colorMap: (StandardSwizzleMaps at: form depth).
		swizzleBlt copyBits.
	].

	hackBlt copyBits.