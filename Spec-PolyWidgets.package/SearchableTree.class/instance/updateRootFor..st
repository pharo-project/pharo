updating
updateRootFor: aText
	| aFilter |
	
	aFilter := TokenTreeFilter token: aText.
	tree filterWith: aFilter