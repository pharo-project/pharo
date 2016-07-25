instance creation
generateFieldsVersion4

	timeLow := self generateRandomBitsOfLength: 32.
	timeMid := self generateRandomBitsOfLength: 16.
	timeHiAndVersion := 16r4000 bitOr: (self generateRandomBitsOfLength: 12).
	clockSeqHiAndReserved := 16r80 bitOr: (self generateRandomBitsOfLength: 6).
	clockSeqLow := self generateRandomBitsOfLength: 8.
	node := self generateRandomBitsOfLength: 48.
	