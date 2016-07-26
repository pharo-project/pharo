modifier state
commandKeyPressed
	"Answer true if the command key on the keyboard was being held down when this event occurred."

	"The mask for command bit pressed"
	^ buttons anyMask: 2r01000000