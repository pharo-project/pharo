current cursor
currentCursor: aCursor 
	"Make the instance of cursor, aCursor, be the current cursor. Display it."

	ActiveHand ifNotNil: [
		ActiveHand world currentCursor: aCursor.
		ActiveHand world isCursorOwner ifTrue: [
			aCursor beCursor
		].
		^ self
	].

	CurrentCursor := aCursor.
	aCursor beCursor