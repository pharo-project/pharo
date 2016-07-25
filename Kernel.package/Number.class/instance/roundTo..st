truncation and round off
roundTo: quantum 
	"Answer the nearest number that is a multiple of quantum."

	^(self / quantum) rounded * quantum