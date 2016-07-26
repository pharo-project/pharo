accessing
timeIfAbsent: aBlock
	"FIX:this is just a hack"

	^ self log timeAt: entry ifAbsent: aBlock