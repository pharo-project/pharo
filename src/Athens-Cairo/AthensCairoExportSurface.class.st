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
