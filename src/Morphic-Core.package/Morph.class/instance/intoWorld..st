initialize
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	aWorld ifNil:[^self].
	self wantsSteps ifTrue:[aWorld startStepping: self].
	self submorphsDo:[:m| m intoWorld: aWorld].
