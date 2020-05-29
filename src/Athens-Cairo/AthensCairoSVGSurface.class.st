"
I represent an Athens surface that writes a SVG file using the cairo graphics library.

Usage:

Use like a AthensCairoSurface with these differences: 
1. On instance creation, specify (a) output filename as String and (b) the extent in points (1 point == 1/72.0 inch).
2. When drawing is done, send #finish to write the file trailer.
3. Let the garbage collector finalize the instance, which will flush to disk (file in disk may have 0 bytes before).

Links: 

https://cairographics.org/manual/cairo-SVG-Surfaces.html
http://zetcode.com/gfx/cairo/cairobackends/
http://www.ie.u-ryukyu.ac.jp/~e085739/c.cairo.3.html

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
