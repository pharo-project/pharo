initialization
on: aCollection from: firstIndex to: lastIndex
	super on: aCollection from: firstIndex to: lastIndex.
	crc := 16rFFFFFFFF.
	expectedCrc := nil.