file menu action
getHex
	"Get contents of file again, and display in Hex. Do this by making the cancel string be the contents, and doing a cancel."

	Cursor read showWhile: [
		brevityState := #needToGetBriefHex.
		self changed: #contents].
