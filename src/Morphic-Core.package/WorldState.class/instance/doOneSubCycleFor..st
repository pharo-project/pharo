update cycle
doOneSubCycleFor: aWorld
	"Like doOneCycle, but preserves activeHand."

	| currentHand |
	currentHand := self activeHand.
	self doOneCycleFor: aWorld.
	self activeHand: currentHand.