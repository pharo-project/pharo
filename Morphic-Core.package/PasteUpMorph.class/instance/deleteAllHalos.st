world state
deleteAllHalos
	"self halt.
	self haloMorphs
		do: [:each | (each target isKindOf: SelectionMorph)
				ifTrue: [each target delete]].
	self hands
		do: [:each | each removeHalo]"
	self flag: #???