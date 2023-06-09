"
I represent the common behavior of special Athens surfaces that render to a file using the cairo graphics library.

The main difference between my subclasses is given by the function that creates the external surface via FFI. See implementors of #createExportingTo:width:height:.

Use me like a AthensCairoSurface with these differences:
1. On instance creation, the caller specifies (a) output filename as String and (b) the extent in points (1 point == 1/72.0 inch).
2. When drawing is done, the caller should send #close to write the file trailer into the library buffer and then flush to disk.

Note 1: Before #close, the file may already exist in disk with 0 bytes.
Note 2: If the garbage collector finalizes the instance before #close, the exported file may have an invalid format.

References:
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
	^ (self createExportingTo: fileName width: anExtent x asFloat height: anExtent y asFloat)
		initializeWith: anExtent;
		yourself
]

{ #category : #'finalize resources' }
AthensCairoExportSurface class >> finalizeResourceData: data [
	"Finalize external state. I override super behavior for two reasons:
	* Export surfaces do not register to the SurfacePlugin, so there is less state to finalize.
	* Support too be executed more than once. This was required because user finalizes the external object when drawing is done and also the Garbage Collector finalizes."

	| surfaceHandle contextHandleValue |
	surfaceHandle := data first.
	contextHandleValue := data second.

	surfaceHandle isNull ifTrue: [ ^self ].
	self destroySurfaceHandle: surfaceHandle value.
	self destroyContextHandle: contextHandleValue.
	surfaceHandle beNull
]

{ #category : #private }
AthensCairoExportSurface class >> registerSurface: surface [
	"Do not register my instances with SurfacePlugin since I only write to file."
]

{ #category : #finalization }
AthensCairoExportSurface >> close [
	"Finish and flush to file the external surface. It is a required step when drawing is done and file should be written to disk."

	self
		finish; "Make the cairo library to write the file trailer into its buffer"
		finalizeResourceData "Destroy surface (and canvas) and flush buffer to disk"
]

{ #category : #accessing }
AthensCairoExportSurface >> extent [
	^ extent
]

{ #category : #finalization }
AthensCairoExportSurface >> finalizeResourceData [
	"Finalize my external state. Read comment on #finalizeResourceData: on class-side."

	self class finalizeResourceData: self resourceData
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

{ #category : #initialization }
AthensCairoExportSurface >> resourceData [
	"See #finalizeResourceData: at class-side."

	^ { handle. context handle }
]

{ #category : #accessing }
AthensCairoExportSurface >> width [
	^ extent x
]
