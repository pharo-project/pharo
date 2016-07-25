accessing
selected: aBoolean
	"Use the appropriate label."

	self removeAllMorphs.
	self isSelected
		ifTrue: [self addMorph: self selectedLabel]
		ifFalse: [self addMorph: self normalLabel]