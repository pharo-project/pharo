initialize
outOfWorld: aWorld
	"Get rid of the list if visible."

	self hideList.
	^super outOfWorld: aWorld