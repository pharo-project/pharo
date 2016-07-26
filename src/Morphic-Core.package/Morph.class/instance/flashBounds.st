drawing
flashBounds
	"Flash the receiver's bounds  -- does not use the receiver's color, thus works with StringMorphs and SketchMorphs, etc., for which #flash is useless.  No senders initially, but useful to send this from a debugger or inspector"

	5 timesRepeat:
		[Display flash: self boundsInWorld  andWait: 120]