testing
testPointer
	| alien address alias |
	alien := Alien newC: 3.
	address := alien address.
	alias := Alien forPointer: address.
	self should: [alias dataSize = 0].
	self should: [alias address = address].
	alien free.