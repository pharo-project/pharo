rendering
renderGlyphsIn: text from: start to: stop
	| face |
	face := font face.
"	face
		setPixelWidth: pixelSize height: pixelSize.
"
	start to: stop do: [:i | | bx by |
		
		bx := penX + bitmapX.
		by := penY + bitmapY.

		"add a small shift, to incorporate subpixel position"
		
		translation at: 1 put: descend x + (bx bitAnd: 2r111111).
		translation at: 2 put: descend y - (by bitAnd: 2r111111). 

		face primSetTransform: matrix delta: translation.

		self loadUnicode: (text at: i) asUnicode.
	
		self clearBitmap.
		face renderGlyphIntoForm: form.

		blt
			destX: bx >> 6;
			destY: by >> 6;
			copyBits.

	"increment x by horizontal advance"
		penX := penX + slot advanceX.
		penY := penY - slot advanceY.

	].

