initialize-release
doOneCycle
	self extraWorldList do: [:world |
		ActiveHand := world activeHand.
		world doOneCycle.
	].

	(World isNotNil and: [(self extraWorldList includes: World) not]) ifTrue: [
		World doOneCycle.
		ActiveHand := World activeHand
	].
	
	