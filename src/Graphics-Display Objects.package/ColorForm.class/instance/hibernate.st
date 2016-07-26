filein/out
hibernate
	"Make myself take up less space. See comment in Form>hibernate."

	super hibernate.
	self clearColormapCache.
	colors ifNotNil:[colors := colors asColorArray].