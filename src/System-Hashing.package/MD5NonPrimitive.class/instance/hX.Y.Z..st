private-functions
hX: x Y: y Z: z
	" compute 'x xor y xor z'"
	^ x copy bitXor: y; bitXor: z

	