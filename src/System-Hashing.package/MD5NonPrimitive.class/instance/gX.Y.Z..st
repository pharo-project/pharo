private-functions
gX: x Y: y Z: z
	" compute 'xz or y(not z)'"
	^ x copy bitAnd: z; bitOr: (z copy bitInvert; bitAnd: y)

	