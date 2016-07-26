update cycle
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	DisplayScreen checkForNewScreenSize.

	"process user input events"
	LastCycleTime := Time millisecondClockValue.
	self handsDo: [:h |
		ActiveHand := h.
		h processEvents.
		ActiveHand := nil
	].

	"the default is the primary hand"
	ActiveHand := self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.