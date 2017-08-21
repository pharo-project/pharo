writting
writeUInt: aNumber
	1 to: 4 do: [ :i | stream nextPut: (aNumber digitAt: i) ]