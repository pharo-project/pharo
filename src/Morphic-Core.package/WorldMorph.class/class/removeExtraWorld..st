extra worlds
removeExtraWorld: aWorld
	ExtraWorldList := self extraWorldList copyWithout: aWorld.
	CursorOwnerWorld == aWorld ifTrue: [ CursorOwnerWorld := nil. ]
	