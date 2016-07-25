stop conditions
plainTab
	| nextDestX |
 	nextDestX := super plainTab.
	fillBlt == nil ifFalse:
		[fillBlt destX: destX destY: destY width: nextDestX - destX height: font height; copyBits].
	^nextDestX