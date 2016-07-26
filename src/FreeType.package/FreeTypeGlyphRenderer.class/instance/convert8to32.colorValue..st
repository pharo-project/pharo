private
convert8to32: aGlyphForm colorValue: foreColorValue
	"convert from the 8 bit deep form produced by FreeType, where each byte represents the intensity of a single pixel, to a 32 bit deep form with pixels of color foreColorValue " 
	| w h s answer rowstart bytes word littleEndian shift v a colorVal foreColorVal foreColorA foreColorR foreColorG foreColorB r g b |

	foreColorVal := foreColorValue.
	foreColorA := foreColorVal >> 24.
	foreColorR := foreColorVal >> 16 bitAnd: 16rFF.
	foreColorG := foreColorVal >> 8 bitAnd: 16rFF.
	foreColorB := foreColorVal bitAnd: 16rFF.
	bytes := aGlyphForm bits.
	w := aGlyphForm width.
	h := aGlyphForm height.
	answer := aGlyphForm class extent: w@h depth: 32.
	answer 
		offset: (aGlyphForm offset x) @ (aGlyphForm offset y);
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
			a := v > 0 ifTrue:[v * foreColorA // 16rFF] ifFalse:[0].
			r := v > 0 ifTrue:[a * foreColorR // 16rFF] ifFalse:[0].
			g := v > 0 ifTrue:[a * foreColorG // 16rFF] ifFalse:[0].
			b := v > 0 ifTrue:[a * foreColorB // 16rFF] ifFalse:[0].
			colorVal := (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b.
			answer bits integerAt: (y*w)+(x+1) put: colorVal]].
	^answer