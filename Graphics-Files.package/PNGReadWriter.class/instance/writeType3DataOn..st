writing
writeType3DataOn: zStream
	"Write color indexed data."
	| scanline hack hackBlt swizzleBlt swizzleHack hackDepth |
	scanline := ByteArray new: bytesPerScanline + 3 // 4 * 4.
	hackDepth := bigEndian ifTrue:[form depth] ifFalse:[form depth negated].
	hack := Form extent: width@1 depth: hackDepth bits: scanline.
	hackBlt := BitBlt toForm: hack.
	hackBlt sourceForm: form.
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@0.
	hackBlt width: width; height: 1.
	(form depth < 8 and:[bigEndian not]) ifTrue:[
		swizzleHack := Form new hackBits: scanline.
		swizzleBlt := BitBlt toForm: swizzleHack.
		swizzleBlt sourceForm: swizzleHack.
		swizzleBlt combinationRule: Form over.
		swizzleBlt colorMap: (StandardSwizzleMaps at: form depth).
	].
	0 to: height-1 do:[:i|
		hackBlt sourceOrigin: 0@i; copyBits.
		swizzleBlt ifNotNil:[swizzleBlt copyBits].
		zStream nextPut: 0. "filterType"
		zStream next: bytesPerScanline putAll: scanline startingAt: 1.
	].
	zStream close.