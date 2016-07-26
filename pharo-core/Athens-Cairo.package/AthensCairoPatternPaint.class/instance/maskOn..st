drawing
maskOn: anAthensCanvas
"
A drawing operator that paints the current source using the alpha channel of pattern as a mask. (Opaque areas of pattern are painted with the source, transparent areas are not painted.)
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_mask(cairo_t anAthensCanvas, self))