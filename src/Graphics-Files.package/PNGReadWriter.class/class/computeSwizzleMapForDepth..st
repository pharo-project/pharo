utilities
computeSwizzleMapForDepth: depth
	"Answer a map that maps pixels in a word to their opposite location. Used for 'middle-endian' forms where the byte-order is different from the bit order (good joke, eh?)."
	| map swizzled |
	map := Bitmap new: 256.
	depth = 4 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 15) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 15) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].

	depth = 2 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 3) bitShift: 6).
			swizzled := swizzled bitOr: (((pix bitShift: -2) bitAnd: 3) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 3) bitShift: 2).
			swizzled := swizzled bitOr: (((pix bitShift: -6) bitAnd: 3) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].

	depth = 1 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 1) bitShift: 7).
			swizzled := swizzled bitOr: (((pix bitShift: -1) bitAnd: 1) bitShift: 6).
			swizzled := swizzled bitOr: (((pix bitShift: -2) bitAnd: 1) bitShift: 5).
			swizzled := swizzled bitOr: (((pix bitShift: -3) bitAnd: 1) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 1) bitShift: 3).
			swizzled := swizzled bitOr: (((pix bitShift: -5) bitAnd: 1) bitShift: 2).
			swizzled := swizzled bitOr: (((pix bitShift: -6) bitAnd: 1) bitShift: 1).
			swizzled := swizzled bitOr: (((pix bitShift: -7) bitAnd: 1) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].
	self error: 'Unrecognized depth'