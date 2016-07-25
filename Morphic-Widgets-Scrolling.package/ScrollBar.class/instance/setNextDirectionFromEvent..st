scrolling
setNextDirectionFromEvent: event

	nextPageDirection := bounds isWide ifTrue: [
		event cursorPoint x >= slider center x
	]
	ifFalse: [
		event cursorPoint y >= slider center y
	]

