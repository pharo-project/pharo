geometry testing
minExtent
	"Answer the minmum extent of the receiver.
	Based on label and rounding."

	^super minExtent max: self labelMorph minExtent + (8@0)