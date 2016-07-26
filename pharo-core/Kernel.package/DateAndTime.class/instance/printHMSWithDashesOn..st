printing
printHMSWithDashesOn: aStream
	"Print just hh-mm-ss"

	self printHMSOn: aStream separatedBy: $-