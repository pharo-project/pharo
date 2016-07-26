initialize
destroySurface
	"Users must call this explicitly when this object is no longer needed; otherwise, resource-leakage will occur in the SurfacePlugin"
	bits ifNotNil: [:surfaceID |
		bits := nil.
		self primDestroyManualSurface: surfaceID
	].