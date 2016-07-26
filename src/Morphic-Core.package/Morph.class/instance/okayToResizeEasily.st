halos and balloon help
okayToResizeEasily
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ true

	"This one was too jarring, not that it didn't most of the time do the right  thing but because some of the time it didn't, such as in a holder.  If we pursue this path, the test needs to be airtight, obviously...
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"