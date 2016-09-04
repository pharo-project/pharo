accessing
findStringFromEnd: string
	"Fast version to find a String in a file starting from the end.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos |
	oldPos := self position.
	self setToEnd.
	pos := self position.
	[ pos := ((pos - 2000 + string size) max: 0).  "the [+ string size] allows for the case where the end of the search string is at the beginning of the current buffer"
	self position: pos.
	buffer := self next: 2000.
	(count := buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count-1.  "use next instead of position:, so that CrLfFileStream can do its magic if it is being used"
			^self position].
	pos = 0] whileFalse.
	"Never found it, and hit beginning of file"
	self position: oldPos.
	^0