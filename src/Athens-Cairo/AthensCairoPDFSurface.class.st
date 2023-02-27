"
I represent an Athens surface that renders into a PDF file using the cairo graphics library.

For more information, please read the comment of my superclass, which implements behavior that is shared with other surfaces that write to file using the cairo graphics library.

Reference: https://cairographics.org/manual/cairo-PDF-Surfaces.html

"
Class {
	#name : #AthensCairoPDFSurface,
	#superclass : #AthensCairoExportSurface,
	#category : #'Athens-Cairo-Surface'
}

{ #category : #private }
AthensCairoPDFSurface class >> createExportingTo: fileName width: width height: height [
	^ self ffiCall: #(AthensCairoPDFSurface cairo_pdf_surface_create(
			String fileName,
			double width,
			double height) )
]
