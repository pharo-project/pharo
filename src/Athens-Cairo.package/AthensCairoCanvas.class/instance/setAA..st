private
setAA: antiAlias

	"cairo_set_antialias ()

void                cairo_set_antialias                 (cairo_t *cr,
                                                         cairo_antialias_t antialias);

Set the antialiasing mode of the rasterizer used for drawing shapes. This value is a hint, and a particular backend may or may not support a particular value. At the current time, no backend supports CAIRO_ANTIALIAS_SUBPIXEL when drawing shapes.

Note that this option does not affect text rendering, instead see cairo_font_options_set_antialias().

"
<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_set_antialias (self, uint antiAlias))
