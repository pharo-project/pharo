accessing
temps
	| result tempOffset numTemps |
	tempOffset := copiedValues size + numArgs.
	numTemps := temps - tempOffset.
	result := Array new: numTemps.
	tempOffset := tempOffset - 1.
	1 to: numTemps do: [ :idx | result at: idx put: self -> (tempOffset + idx)  ].
	ownTempVectors do: [ :tempVector | result at: tempVector index - tempOffset put: tempVector ].
	^ result