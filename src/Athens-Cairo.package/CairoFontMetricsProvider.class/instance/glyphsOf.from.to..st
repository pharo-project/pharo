private
glyphsOf: aString from: start to: end
	| len ptr glyphs lenValue glyphsSize utf8Len error |

	len := end-start+1.
	utf8Len := utfConverter convert: aString from: start to: end.

	ptr := ExternalAddress new.
	lenValue := ByteArray new: 4.
	lenValue nbUInt32AtOffset: 0 put: len.
		
	error := self 
		convertString: utfConverter buffer 
		len: utf8Len
		ofFont: cairoFont 
		toGlyphs: ptr
		numGlyphs: lenValue 
		x: 0.0 
		y: 0.0.
		
	error = CAIRO_STATUS_SUCCESS ifFalse: [  ^ CairoGlyphsArray new: 0 ].

	glyphsSize := lenValue nbUInt32AtOffset: 0.
   cairoFont getExtentsOfGlyphs: ptr ofLength: glyphsSize into: glyphExtents.
	
	glyphs := CairoGlyphsArray new: glyphsSize.
	LibC memCopy: ptr to: glyphs getHandle size: (glyphsSize * glyphs typeSize).
	"Apply kerning on glyphs if font supports it"
	font face hasKerning
		ifTrue: [ self applyKerningOn: glyphs of: aString from: start to: end ].
	self freeGlyphs: ptr.
		
	^ glyphs 