*Graphics-Primitives
pixelScreenForDepth: depth 
	"Return a 50% stipple containing alternating pixels of all-zeros and all-ones to be used as a mask at the given depth."
	| mask bits |
	mask := (1 bitShift: depth) - 1.
	bits := 2 * depth.
	[ bits >= 32 ] whileFalse: 
		[ mask := mask bitOr: (mask bitShift: bits).	"double the length of mask"
		bits := bits + bits ].
	^ Bitmap 
		with: mask
		with: mask bitInvert32