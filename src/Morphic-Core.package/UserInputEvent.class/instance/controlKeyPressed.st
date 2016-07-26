modifier state
controlKeyPressed
	"Answer true if the control key on the keyboard was being held down when this event occurred."

	"The mask for control bit pressed"
	^ buttons anyMask: 2r00010000