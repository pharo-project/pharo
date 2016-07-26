initialize
initialize 
	utfConverter := CairoUTF8Converter new.
	advance := 0@0.
	cache := CairoBackendCache soleInstance.
	glyphExtents := CairoTextExtents new.