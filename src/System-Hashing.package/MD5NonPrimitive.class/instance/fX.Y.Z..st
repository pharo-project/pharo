private-functions
fX: x Y: y Z: z
	" compute 'xy or (not x)z'"
	^ x copy bitAnd: y; bitOr: (x copy bitInvert; bitAnd: z)

	