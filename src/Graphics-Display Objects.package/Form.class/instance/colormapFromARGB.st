color mapping
colormapFromARGB
	"Return a ColorMap mapping from canonical ARGB space into the receiver.
	Note: This version is optimized for Squeak forms."
	| map nBits |
	self hasNonStandardPalette 
		ifTrue:[^ColorMap mappingFromARGB: self rgbaBitMasks].
	self depth <= 8 ifTrue:[
		map := Color colorMapIfNeededFrom: 32 to: self depth.
		map size = 512 ifTrue:[nBits := 3].
		map size = 4096 ifTrue:[nBits := 4].
		map size = 32768 ifTrue:[nBits := 5].
		^ColorMap
			shifts: (Array 
						with: 3 * nBits - 24
						with: 2 * nBits - 16
						with: 1 * nBits - 8
						with: 0)
			masks: (Array
						with: (1 << nBits) - 1 << (24 - nBits)
						with: (1 << nBits) - 1 << (16 - nBits)
						with: (1 << nBits) - 1 << (8 - nBits)
						with: 0)
			colors: map].
	self depth = 16 ifTrue:[
		^ColorMap
			shifts: #(-9 -6 -3 0)
			masks: #(16rF80000 16rF800 16rF8 0)].
	self depth = 32 ifTrue:[
		^ColorMap
			shifts: #(0 0 0 0)
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000)].
	self error:'Bad depth'