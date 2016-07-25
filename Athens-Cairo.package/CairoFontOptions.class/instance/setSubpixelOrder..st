subpixel order
setSubpixelOrder: order
	"void cairo_font_options_set_subpixel_order
		(cairo_font_options_t *options,
		cairo_subpixel_order_t subpixel_order)"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(
		void cairo_font_options_set_subpixel_order (self, uint order))
 