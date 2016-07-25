private
getPositionsList

	^ self item 
		ifNil: [ {} ]
		ifNotNil: [:it | it first possiblePositions ]