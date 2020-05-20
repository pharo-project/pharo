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
	#superclass : #AthensCairoSurface,
	#category : #'Athens-Cairo-Surface'
}

{ #category : #private }
AthensCairoSVGSurface class >> createSVG: fileName width: width_in_points height: height_in_points [
	^ self ffiCall: #(AthensCairoSVGSurface cairo_svg_surface_create (String fileName,
                                                      double width_in_points,
                                                      double height_in_points) )

]

{ #category : #'instance creation' }
AthensCairoSVGSurface class >> extent: anExtent fileName: fileName [
	^ (self createSVG: fileName width: anExtent x height: anExtent y)
		initialize;
		yourself
]

{ #category : #'finalize resources' }
AthensCairoSVGSurface class >> finalizeResourceData: data [
	| handle contextHandle |
	handle := data first.
	contextHandle := data third.

	self destroyContextHandle: contextHandle value. 
	self destroySurfaceHandle: handle value

]

{ #category : #private }
AthensCairoSVGSurface class >> registerSurface: surface [
	"do not register my instances with SurfacePlugin"
]
