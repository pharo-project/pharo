control
popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	^self popUpAt: aWorld primaryHand position forHand: aWorld primaryHand in: aWorld
