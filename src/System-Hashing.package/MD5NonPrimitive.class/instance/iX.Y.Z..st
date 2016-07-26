private-functions
iX: x Y: y Z: z
	" compute 'y xor (x or (not z))'"
	^ y copy bitXor: (z copy bitInvert; bitOr: x)
