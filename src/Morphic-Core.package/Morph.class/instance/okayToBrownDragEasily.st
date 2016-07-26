halos and balloon help
okayToBrownDragEasily
	"Answer whether it it okay for the receiver to be brown-dragged easily -- i.e. repositioned within its container without extracting it.  At present this is just a hook -- nobody declines."

	^ true



"
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"