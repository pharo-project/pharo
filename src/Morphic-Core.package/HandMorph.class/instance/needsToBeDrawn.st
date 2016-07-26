drawing
needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor and shadow from the display."
	| cursor |
	(savedPatch notNil
		or: [ (submorphs anySatisfy: [ :ea | ea visible ])
			or: [ (temporaryCursor notNil and: [hardwareCursor isNil])
				]])
		ifTrue: [
			"using the software cursor; hide the hardware one"
			Cursor blank isCurrent ifFalse: [Cursor blank show].
			^ true].
	"Switch from one hardware cursor to another, if needed."
	cursor := hardwareCursor ifNil: [Cursor normal].
	cursor isCurrent ifFalse: [cursor show].
	^ false
