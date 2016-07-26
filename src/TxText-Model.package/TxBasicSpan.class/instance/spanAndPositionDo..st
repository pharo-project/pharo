enumerating
spanAndPositionDo:  aBlock
	| position |
	position := 0.
	self do: [ :span|
		aBlock value: span value: position.
		position := position + span size ]