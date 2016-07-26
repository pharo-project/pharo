pixel copies
copyPixels: y at: startX by: incX 
	"Handle interlaced pixels of supported colorTypes"
	| s |
	s := #(
		#copyPixelsGray:at:by:
		nil
		#copyPixelsRGB:at:by:
		#copyPixelsIndexed:at:by:
		#copyPixelsGrayAlpha:at:by:
		nil
		#copyPixelsRGBA:at:by:
	) at: colorType + 1.
	self 
		perform: s asSymbol
		with: y
		with: startX
		with: incX