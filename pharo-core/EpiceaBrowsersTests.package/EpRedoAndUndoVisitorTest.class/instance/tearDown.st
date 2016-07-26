running
tearDown

	super tearDown.
	
	EpMiniMonticelloResource current workingPatch isEmpty ifFalse: [
		EpMiniMonticelloResource current revert ]