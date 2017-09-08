initialize - release
stateArrayPut: stateArray
	| sel |
	sel := stateArray at: 1.
	self selectFrom: sel first to: sel last.
	beginTypeInIndex := stateArray at: 2