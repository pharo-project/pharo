private-receiver-guessing
isVariablesOnly
	| current previous |
	current := ranges at: completionIndex.
	^ current isVariablesOnly or: 
		[ current isOpening or: 
			[ previous := ranges at: completionIndex - 1.
			previous isOpening or: 
				[ previous isSeparator or: 
					[ previous isKeyword or: [ previous isAssignment or: [ previous isBinary ] ] ] ] ] ]