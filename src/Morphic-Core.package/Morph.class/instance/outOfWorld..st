initialize
outOfWorld: aWorld
	"The receiver has just appeared in a new world. Notes:
		* aWorld can be nil (due to optimizations in other places)
		* owner is still valid
	Important: Keep this method fast - it is run whenever morphs are removed."
	aWorld ifNil:[^self].
	"ar 1/31/2001: We could explicitly stop stepping the receiver here but for the sake of speed I'm for now relying on the lazy machinery in the world itself."
	"aWorld stopStepping: self."
	self submorphsDo:[:m| m outOfWorld: aWorld].
