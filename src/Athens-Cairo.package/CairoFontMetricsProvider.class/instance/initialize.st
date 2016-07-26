initialization
initialize
	utfConverter := CairoUTF8Converter new.
	glyphExtents := CairoTextExtents new.
	cache := CairoBackendCache soleInstance.