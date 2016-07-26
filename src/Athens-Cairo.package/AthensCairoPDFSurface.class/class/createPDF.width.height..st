private
createPDF: fileName width: width_in_points height: height_in_points

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensCairoPDFSurface cairo_pdf_surface_create (String fileName,
                                                      double width_in_points,
                                                      double height_in_points) )
