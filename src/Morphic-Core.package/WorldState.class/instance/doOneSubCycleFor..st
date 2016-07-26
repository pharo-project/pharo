update cycle
doOneSubCycleFor: aWorld
	"Like doOneCycle, but preserves activeHand."

	| currentHand |
	currentHand := ActiveHand.
	self doOneCycleFor: aWorld.
	ActiveHand := currentHand