"
I represent the common behavior of special Athens surfaces that render to a file using the cairo graphics library.

The main difference between my subclasses is given by the function to create the external surface via FFI.

Usage:

Use like a AthensCairoSurface with these differences: 
1. On instance creation, specify (a) output filename as String and (b) the extent in points (1 point == 1/72.0 inch).
2. When drawing is done, send #finish to write the file trailer.
3. Let the garbage collector finalize the instance, which will flush to disk (file in disk may have 0 bytes before).

Links: 

* http://zetcode.com/gfx/cairo/cairobackends/
* http://www.ie.u-ryukyu.ac.jp/~e085739/c.cairo.3.html
* https://cairographics.org/manual/cairo-PDF-Surfaces.html
* https://cairographics.org/manual/cairo-SVG-Surfaces.html

"
Class {
	#name : #AthensCairoExportSurface,
	#superclass : #AthensCairoSurface,
	#instVars : [
		'extent'
	],
	#category : #'Athens-Cairo-Surface'
}

{ #category : #private }
AthensCairoExportSurface class >> createExportingTo: fileName width: width height: height [
	^ self subclassResponsibility
]

{ #category : #'instance creation' }
AthensCairoExportSurface class >> extent: anExtent fileName: fileName [
	^ (self createExportingTo: fileName width: anExtent x height: anExtent y)
		initializeWith: anExtent;
		yourself
]

{ #category : #'finalize resources' }
AthensCairoExportSurface class >> finalizeResourceData: data [
	| handle contextHandle |
	handle := data first.
	contextHandle := data third.

	self destroyContextHandle: contextHandle value. 
	self destroySurfaceHandle: handle value

]

{ #category : #private }
AthensCairoExportSurface class >> registerSurface: surface [
	"do not register my instances with SurfacePlugin"
]

{ #category : #accessing }
AthensCairoExportSurface >> extent [
	^ extent
]

{ #category : #accessing }
AthensCairoExportSurface >> height [
	^ extent y
]

{ #category : #initialization }
AthensCairoExportSurface >> initializeWith: anExtent [
	self initialize.
	
	"We keep the extent to override related accessors because superclass uses cairo_image_surface_get_width and similar for height which don't give the right answer."
	extent := anExtent
]

{ #category : #accessing }
AthensCairoExportSurface >> width [
	^ extent x
]
