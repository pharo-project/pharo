accessing
percentage
	"Answer the percentage of total that has already been transferred.
	Can be nil. Should be between 0 and 100."

	^ self isEmpty ifFalse: [ self fraction * 100 ]