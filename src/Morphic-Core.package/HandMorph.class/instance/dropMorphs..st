grabbing/dropping
dropMorphs: anEvent
	"Drop the morphs at the hands position"
	self submorphsReverseDo:[:m|
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent.
	].