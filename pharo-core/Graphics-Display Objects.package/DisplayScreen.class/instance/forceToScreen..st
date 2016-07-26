other
forceToScreen: aRectangle
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Typically used when the deferUpdates flag in the virtual machine is on; see deferUpdates:."

	self primShowRectLeft: aRectangle left
		right: aRectangle right
		top: aRectangle top
		bottom: aRectangle bottom.
