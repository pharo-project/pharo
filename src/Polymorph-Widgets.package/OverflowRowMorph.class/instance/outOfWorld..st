initialize
outOfWorld: aWorld
	"Get rid of the more column if visible."

	self hideMore.
	^super outOfWorld: aWorld