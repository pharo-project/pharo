accessing
surface: aCairoSurface
	
	"initialize for given surface"
	surface := aCairoSurface.
	
	pathTransform := AthensCairoMatrix new.
	paintTransform := AthensCairoMatrix new.
	paintMode := AthensCairoPaintMode new canvas: self.
	
	self setAA: CAIRO_ANTIALIAS_SUBPIXEL.