as yet unclassified
primImage: aFormat width: aWidth height: aHeight
"Creates a Quartz surface backed by a CGBitmap. The surface is created using the Device RGB (or Device Gray, for A8) color space. All Cairo operations, including those that require software rendering, will succeed on this surface. "

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensQuartzSurface  cairo_quartz_surface_create (int aFormat,
                                                         int aWidth,
                                                         int aHeight) )

