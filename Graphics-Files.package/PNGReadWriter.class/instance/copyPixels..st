pixel copies
copyPixels: y 
	"Handle non-interlaced pixels of supported colorTypes"
	| s |
	s := #(
		#copyPixelsGray:
		nil
		#copyPixelsRGB:
		#copyPixelsIndexed:
		#copyPixelsGrayAlpha:
		nil
		#copyPixelsRGBA:
	) at: colorType + 1.
	self 
		perform: s asSymbol
		with: y