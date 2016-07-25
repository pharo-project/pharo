*Glamour-Morphic-Brick
asRGBInteger

	^(self red * 255) asInteger + ((self green * 255) asInteger << 8) + ((self blue * 255) asInteger << 16)