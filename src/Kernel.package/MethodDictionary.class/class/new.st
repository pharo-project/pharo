instance creation
new
	"Create a new instance with 32 slots, which can hold at most 24 methods before growing is necessary."
	
	^self newForCapacity: 32