accessing
fontNamesAndSizes  "TextStyle default fontNames"
	^ fontArray collect: [:x | x name, ' ', x height printString]