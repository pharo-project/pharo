position
position: aSymbol

	(self possiblePositions includes: aSymbol) ifFalse: [ ^ self ].

	Position := aSymbol