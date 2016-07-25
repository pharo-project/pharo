private
pixelHeight
	^ pixelHeight
		ifNil: [pixelHeight := (font
						ifNil: [TextStyle defaultFont]) pixelSize]