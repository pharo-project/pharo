construction
addLine
	"Append a divider line to this menu. Suppress duplicate lines."

	submorphs isEmpty ifTrue: [^ self].
	(self lastSubmorph isKindOf: DockingBarMenuLineMorph)
		ifFalse: [self addMorphBack: DockingBarMenuLineMorph new].
