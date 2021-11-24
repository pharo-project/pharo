"
I represent an Athens surface that renders into a SVG file using the cairo graphics library.

For more information, please read the comment of my superclass, which implements behavior that is shared with other surfaces that write to file using the cairo graphics library.

Reference: https://cairographics.org/manual/cairo-SVG-Surfaces.html

"
Class {
	#name : #AthensCairoSVGSurface,
	#superclass : #AthensCairoExportSurface,
	#category : #'Athens-Cairo-Surface'
}

{ #category : #private }
AthensCairoSVGSurface class >> createExportingTo: fileName width: width height: height [
	^ self ffiCall: #(AthensCairoSVGSurface cairo_svg_surface_create (
				String fileName,
				double width,
				double height) )
]
