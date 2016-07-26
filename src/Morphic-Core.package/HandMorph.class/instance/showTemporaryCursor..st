cursor
showTemporaryCursor: cursorOrNil
	"Set the temporary cursor to the given Form. If the argument is nil, revert to the normal cursor."

	self showTemporaryCursor: cursorOrNil hotSpotOffset: 0@0
