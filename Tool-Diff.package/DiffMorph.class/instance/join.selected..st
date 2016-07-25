actions
join: aJoin selected: aBoolean
	"Set the selection for the given join and update the
		src dst and join morphs."

	aJoin selected: aBoolean.
	self srcMorph changed.
	self joinMorph changed.
	self dstMorph changed