private
convert8To32: aGlyphForm
		"convert aGlyphForm from the 8 bit deep form produced by FreeType, where each byte represents the intensity of a single pixel, to a 32 bit deep form" 
	| w h s answer rowstart bytes word littleEndian shift v a colorVal |

	bytes := aGlyphForm bits.
	w := aGlyphForm width.
	h := aGlyphForm height.
	answer := aGlyphForm class extent: w@h depth: 32.
	answer 
		offset: (aGlyphForm offset x) @(aGlyphForm offset y);
		advance: aGlyphForm advance;
		linearAdvance: aGlyphForm linearAdvance.
	s := w + 3 >> 2.
	littleEndian := aGlyphForm isLittleEndian.
	0 to: h - 1 do: [:y |
		rowstart := (y * s)+1.
		0 to: w - 1 do:[:x | 
			word := bytes at: rowstart + (x//4).
			shift := 8* (littleEndian 
				ifTrue:[x bitAnd: 3] 
				ifFalse:[3-(x bitAnd: 3)]).
			v := word >>shift bitAnd: 16rFF.
			a := v > 0 ifTrue:[16rFF] ifFalse:[0].
			colorVal := v + (v bitShift: 8) +  (v bitShift: 16) + (a bitShift: 24).
			answer bits integerAt: (y*w)+(x+1) put: colorVal]].
	^answer