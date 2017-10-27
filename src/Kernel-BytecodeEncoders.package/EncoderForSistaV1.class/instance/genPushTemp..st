bytecode generation
genPushTemp: tempIndex
	(tempIndex < 0 or: [tempIndex > 63]) ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 12 ifTrue: 
		["64-71		01000 i i i		Push Temporary Variable #iii
		   72-75	010010 i i		Push Temporary Variable #ii + 8"
		 stream nextPut: 64 + tempIndex.
		 ^self].
	"229		11100101	i i i i i i i i	Push Temporary Variable #iiiiiiii"
	stream
		nextPut: 229;
		nextPut: tempIndex