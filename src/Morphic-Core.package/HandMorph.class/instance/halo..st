halo handling
halo: newHalo
	"Set halo associated with this hand"
	| oldHalo |
	oldHalo := self halo.
	(oldHalo isNil or:[oldHalo == newHalo]) ifFalse:[oldHalo delete].
	newHalo
		ifNil:[self removeProperty: #halo]
		ifNotNil:[self setProperty: #halo toValue: newHalo]