menu
removeFlexShell
	"Remove the shell used to make a morph rotatable and scalable."

	| oldHalo unflexed myWorld refPos aPosition |
	refPos := self referencePosition.
	myWorld := self world.
	oldHalo := self halo.
	submorphs isEmpty ifTrue: [^ self delete].
	aPosition := (owner submorphIndexOf: self) ifNil: [1].
	unflexed := self firstSubmorph.
	self submorphs do: [:m |
		m position: self center - (m extent // 2).
		owner addMorph: m asElementNumber: aPosition].
	oldHalo ifNotNil: [oldHalo setTarget: unflexed].
	myWorld ifNotNil: [myWorld startSteppingSubmorphsOf: unflexed].
	self delete.
	unflexed referencePosition: refPos.
	^ unflexed