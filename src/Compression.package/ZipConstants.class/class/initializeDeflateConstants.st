pool initialization
initializeDeflateConstants

	WindowSize := 16r8000.
	WindowMask := WindowSize - 1.
	MaxDistance := WindowSize.

	MinMatch := 3.
	MaxMatch := 258.

	HashBits := 15.
	HashMask := (1 << HashBits) - 1.
	HashShift := (HashBits + MinMatch - 1) // MinMatch.
