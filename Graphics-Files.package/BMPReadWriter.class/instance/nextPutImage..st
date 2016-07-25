writing
nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image ppw scanLineLen pixline |
	depth := aForm depth.
	depth := #(1 4 8 32 ) detect: [ :each | each >= depth].
	image := aForm asFormOfDepth: depth.
	image unhibernate.
	bhSize := 14.  "# bytes in file header"
	biSize := 40.  "info header size in bytes"
	biWidth := image width.
	biHeight := image height.
	biClrUsed := depth = 32 ifTrue: [0] ifFalse:[1 << depth].  "No. color table entries"
	bfOffBits := biSize + bhSize + (4*biClrUsed).
	rowBytes := ((depth min: 24) * biWidth + 31 // 32) * 4.
	biSizeImage := biHeight * rowBytes.

	"Write the file header"
	stream position: 0.
	stream nextLittleEndianNumber: 2 put: 19778.  "bfType = BM"
	stream nextLittleEndianNumber: 4 put: bfOffBits + biSizeImage.  "Entire file size in bytes"
	stream nextLittleEndianNumber: 4 put: 0.  "bfReserved"
	stream nextLittleEndianNumber: 4 put: bfOffBits.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextLittleEndianNumber: 4 put: biSize.  "info header size in bytes"
	stream nextLittleEndianNumber: 4 put: image width.  "biWidth"
	stream nextLittleEndianNumber: 4 put: image height.  "biHeight"
	stream nextLittleEndianNumber: 2 put: 1.  "biPlanes"
	stream nextLittleEndianNumber: 2 put: (depth min: 24).  "biBitCount"
	stream nextLittleEndianNumber: 4 put: 0.  "biCompression"
	stream nextLittleEndianNumber: 4 put: biSizeImage.  "size of image section in bytes"
	stream nextLittleEndianNumber: 4 put: 2800.  "biXPelsPerMeter"
	stream nextLittleEndianNumber: 4 put: 2800.  "biYPelsPerMeter"
	stream nextLittleEndianNumber: 4 put: biClrUsed.
	stream nextLittleEndianNumber: 4 put: 0.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [:i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [:j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth < 32 ifTrue: [
		"depth = 1, 4 or 8."
		data := image bits asByteArray.
		ppw := 32 // depth.
		scanLineLen := biWidth + ppw - 1 // ppw * 4.  "# of bytes in line"
		1 to: biHeight do: [:i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	] ifFalse: [
		data := image bits.
		pixline := ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do:[:i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure'].
	stream close.