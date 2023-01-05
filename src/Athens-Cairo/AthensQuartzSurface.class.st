"
The Quartz surface is used to render cairo graphics targeting the Apple OS X Quartz rendering system.

"
Class {
	#name : #AthensQuartzSurface,
	#superclass : #AthensCairoSurface,
	#category : #'Athens-Cairo-Quartz'
}

{ #category : #finalization }
AthensQuartzSurface class >> finalizeResourceData: data [
	| handle contextHandle |
	handle := data first.
	"	id := data second.  - id is not used "
	contextHandle := data third.

	self destroyContextHandle: contextHandle value.
	self destroySurfaceHandle: handle value
]

{ #category : #primitives }
AthensQuartzSurface class >> primImage: aFormat width: aWidth height: aHeight [
"Creates a Quartz surface backed by a CGBitmap. The surface is created using the Device RGB (or Device Gray, for A8) color space. All Cairo operations, including those that require software rendering, will succeed on this surface. "
	^ self ffiCall: #(AthensQuartzSurface  cairo_quartz_surface_create (int aFormat,
                                                         int aWidth,
                                                         int aHeight) )
]

{ #category : #private }
AthensQuartzSurface class >> registerSurface: quartzSurface [
	"we cannot use SurfacePlugin for quartz surfaces"

	^ nil
]
