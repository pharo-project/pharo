convenience
newText: aString
	
	text := TxModel new.
	
	text endPosition insert: aString.
	selection := text endPosition asSelection.
	cursor := selection end.