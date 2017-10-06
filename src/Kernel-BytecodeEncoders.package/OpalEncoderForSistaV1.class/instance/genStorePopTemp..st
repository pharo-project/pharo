bytecode generation
genStorePopTemp: tempIndex
	"208-215	11010 iii			Pop and Store Temporary Variable #iii
	 242		11110010	iiiiiiii	Pop and Store Temporary Variable #iiiiiiii"
	(tempIndex < 0 or: [tempIndex > 63]) ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 8 ifTrue:
		[stream nextPut: 208 + tempIndex.
		 ^self].
	stream
		nextPut: 242;
		nextPut: tempIndex