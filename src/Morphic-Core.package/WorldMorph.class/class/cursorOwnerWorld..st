extra worlds
cursorOwnerWorld: aWorld
	CursorOwnerWorld ~~ aWorld ifTrue: [ 
		CursorOwnerWorld := aWorld.
		(CursorOwnerWorld currentCursor ifNil: [Cursor normal]) beCursor
	]
