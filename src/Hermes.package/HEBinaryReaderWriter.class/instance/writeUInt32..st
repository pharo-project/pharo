writting
writeUInt32: aNumber
	1 to: 4 do: [ :i | stream nextPut: (aNumber digitAt: i) ]