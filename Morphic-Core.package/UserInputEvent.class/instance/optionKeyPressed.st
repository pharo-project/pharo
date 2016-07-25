modifier state
optionKeyPressed
	"Answer true if the alt/option key on the keyboard was being held down when this event occurred."

	"00100000 is the bit that marks that option/alt has been pressed"
	^ buttons anyMask: 2r00100000