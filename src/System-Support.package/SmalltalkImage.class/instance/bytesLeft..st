memory space
bytesLeft: aBool
	"Return the amount of available space. If aBool is true, include possibly available swap space. If aBool is false, include possibly available physical memory. For a report on the largest free block currently availabe within Pharo memory but not counting extra memory use #primBytesLeft."
	<primitive: 112>
	^self primBytesLeft