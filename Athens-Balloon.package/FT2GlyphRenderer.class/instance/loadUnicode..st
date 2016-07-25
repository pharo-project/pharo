private
loadUnicode: unicode

	| ext hintingFlags flags arr face |
	
	face := font face.

"	hintingFlags := FreeTypeSettings current hintingFlags."
	flags :=  LoadNoBitmap bitOr: 2 "hintingFlags".
"	bitOr:( LoadIgnoreTransform  bitOr: 2 ). "
	face primLoadCharacter: unicode flags: flags.

	self loadSlotInfo.
