filein/out
readFromOldFormat: aBinaryStream
	"Read a Form in the original ST-80 format."

	| w h offsetX offsetY newForm theBits pos |
	self error: 'this method must be updated to read into 32-bit word bitmaps'.
	w := aBinaryStream nextWord.
	h := aBinaryStream nextWord.
	offsetX  := aBinaryStream nextWord.
	offsetY := aBinaryStream nextWord.
	offsetX > 32767 ifTrue: [offsetX := offsetX - 65536].
	offsetY > 32767 ifTrue: [offsetY := offsetY - 65536].
	newForm := Form extent: w @ h offset: offsetX @ offsetY.
	theBits := newForm bits.
	pos := 0.
	1 to: w + 15 // 16 do: [:j |
		1 to: h do: [:i |
			theBits at: (pos := pos+1) put: aBinaryStream nextWord]].
	newForm bits: theBits.
	^ newForm
