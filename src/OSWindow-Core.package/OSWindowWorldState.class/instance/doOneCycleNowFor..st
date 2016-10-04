as yet unclassified
doOneCycleNowFor: aWorld
	"process user input events"
	LastCycleTime := Time millisecondClockValue.
	self handsDo: [:h |
		self activeHand: h.
		h processEventsFromQueue: nil.
		self activeHand: nil.
	].

	"the default is the primary hand"
	self activeHand: self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.