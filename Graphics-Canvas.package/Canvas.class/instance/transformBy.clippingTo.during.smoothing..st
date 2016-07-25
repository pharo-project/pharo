drawing-support
transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize
	"Transform the receiver by the given display transformation during the execution of aBlock. The given clip rectangle defines the *global* (e.g., outer) rectangle against which the receiver should clip (which would be equivalent to 'self clipRect: aClipRect; transformBy: aDisplayTransform')."
	^ self subclassResponsibility