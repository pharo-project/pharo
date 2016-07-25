initialize
addTemps: newKeys
	| i offset |
	offset := self currentScope tempMap size.
	i := 1.
	newKeys do: [:key |
		self  currentScope tempMap at: key ifAbsentPut: [i:=i+1.offset + i - 1].
	].
